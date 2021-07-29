/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqaclient

import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.Trace
import org.restlet.data.ChallengeScheme
import org.restlet.data.MediaType

import java.io.ByteArrayOutputStream
import java.io.File
import java.util.Date
import scala.annotation.tailrec

/**
  * Group series into sets of data that can be processed and upload to the AQA platform.
  */

object Upload extends Logging {

  /**
    * Semaphore for maintaining atomicity of update function.
    */
  private val updateSync = ""

  /**
    * A complete set of DICOM series that can be uploaded.  A set of series may consist of any of these:
    *
    *     - A CT series                 (with or without an RTPLAN)
    *     - A CT series and REG series  (with or without an RTPLAN)
    *     - An RTIMAGE series           (with or without an RTPLAN)
    */
  case class UploadSet(procedure: Procedure, imageSeries: Series, reg: Option[Series] = None, plan: Option[Series] = None) {
    override def toString: String = {
      val r = {
        if (procedure.isBBbyCBCT) {
          if (reg.isDefined) "    with reg" else "    no reg"
        } else ""
      }
      val p = if (plan.isDefined) "with plan" else "no plan"
      procedure.toString + "  " + imageSeries.toString + "    PatientID: " + imageSeries.PatientID + r + "    " + p
    }

    /**
      * Get a list of all files in this upload set.
      */
    def getAllDicomFiles: Seq[File] = {
      def filesOf(series: Option[Series]): Seq[File] =
        if (series.isDefined) {
          series.get.ensureFilesExist()
          ClientUtil.listFiles(series.get.dir)
        } else Seq[File]()

      val regFiles: Seq[File] = {
        if (reg.isDefined) {
          reg.get.ensureFilesExist()
          val regFileList = ClientUtil.listFiles(reg.get.dir)
          regFileList
        } else Seq[File]()
      }

      filesOf(Some(imageSeries)) ++ regFiles ++ filesOf(plan)
    }
  }

  private def makeZipFile(fileList: Seq[File]): File = {
    val out = new ByteArrayOutputStream
    FileUtil.readFileTreeToZipStream(fileList, Seq[String](), Seq[File](), out)
    val zipFileName = FileUtil.replaceInvalidFileNameCharacters(edu.umro.ScalaUtil.Util.dateToText(new Date) + ".zip", '_')
    val zipFile = new File(ClientConfig.zipDir, zipFileName)
    FileUtil.writeBinaryFile(zipFile, out.toByteArray)
    zipFile
  }

  /**
    * Send the zip file to the AQA server. Return true on success.  There may later be a failure on
    * the server side, but this success just indicates that the upload was successful.
    */
  private def uploadToAQA(procedure: Procedure, series: Series, zipFile: File): Option[String] = {
    try {
      val start = System.currentTimeMillis
      logger.info("Starting upload of data set to AQA for procedure " + procedure.Name + "    PatientID: " + series.PatientID)

      // upload the files and wait for the processing to finish.  Do this in a synchronized so that no
      // other HTTP activity from this service is being attempted while it waits.
      val result = ClientUtil.sync.synchronized {
        HttpsClient.httpsPostSingleFileAsMulipartForm(
          procedure.URL,
          zipFile,
          MediaType.APPLICATION_ZIP,
          ClientConfig.AQAUser,
          ClientConfig.AQAPassword,
          ChallengeScheme.HTTP_BASIC,
          trustKnownCertificates = true,
          ClientConfig.httpsClientParameters,
          timeout_ms = ClientConfig.HttpsUploadTimeout_ms
        )
      }
      val elapsed = System.currentTimeMillis - start
      result match {
        case Right(good) =>
          logger.info("Elapsed time in ms of upload: " + elapsed + "  Successful upload of data set to AQA for procedure " + procedure.Name + "    PatientID: " + series.PatientID)
          println(good)
          None
        case Left(failure) =>
          logger.warn("Elapsed time in ms of upload: " + elapsed + "  Failed to upload data set to AQA for procedure " + procedure.Name + "    PatientID: " + series.PatientID + " : " + fmtEx(failure))
          Some("Elapsed time in ms of upload: " + elapsed + "  failed to upload data set to AQA: " + failure.getMessage)
      }
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error while using HTTPS client to upload zip file to AQA: " + fmtEx(t))
        Some("Error communicating with AQA: " + fmtEx(t))
    }
  }

  /** Maximum number of times to attempt an upload before giving up. */
  private val maxUploadRetryCount = 4

  /**
    * Upload a set of DICOM files for processing.
    */
  @tailrec
  def upload(uploadSet: UploadSet, retryCount: Int = maxUploadRetryCount): Boolean = {
    logger.info("Processing upload " + uploadSet)
    val success: Boolean =
      try {
        val allDicomFiles = uploadSet.getAllDicomFiles // gather DICOM files from all series
        val zipFile = makeZipFile(allDicomFiles)
        logger.info("Beginning upload of " + uploadSet)
        val msg = uploadToAQA(uploadSet.procedure, uploadSet.imageSeries, zipFile)
        logger.info("Finished upload of " + uploadSet)
        Sent.add(new Sent(uploadSet, msg))
        if (msg.isEmpty)
          logger.info("Successfully uploaded " + uploadSet)
        else {
          logger.warn("Failure while uploading " + uploadSet + " : " + msg.get)
        }
        val ok = msg.isEmpty
        Results.refreshPatient(uploadSet.imageSeries.PatientID)

        Thread.sleep((ClientConfig.GracePeriod_sec * 1000).toLong)
        Series.removeObsoleteZipFiles() // clean up any zip files
        ok
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected exception during upload: " + fmtEx(t))
          false
      }

    if ((!success) && (retryCount > 0)) {
      Thread.sleep(5 * 1000)
      logger.warn("Retrying upload.  Retry count: " + retryCount + "    uploadSet: " + uploadSet)
      upload(uploadSet, retryCount - 1)
    } else success
  }

  /**
    * If the given CT's frame of reference matches an RTPLAN, then upload it.
    */
  private def connectWithPlanByFrameOfRef(ct: Series): Option[UploadSet] = {
    if (ct.isModality(ModalityEnum.CT)) {
      val localPlan = Series.getRtplanByFrameOfReference(ct.FrameOfReferenceUID.get)
      val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, ct.FrameOfReferenceUID.get)

      val procedureOfSeries = PatientProcedure.getProcedureOfSeries(ct)

      (localPlan, remotePlan) match {
        case (Some(rtplan), _) if procedureOfSeries.isDefined => Some(UploadSet(procedureOfSeries.get, ct, Some(rtplan))) // upload CT and RTPLAN
        case (_, true) if procedureOfSeries.isDefined         => Some(UploadSet(procedureOfSeries.get, ct)) // upload just the CT.  The RTPLAN has already been uploaded
        case _                                                => None // no plan available that has the same frame of reference as this CT
      }
    } else
      None
  }

  /**
    * If there is a CT-REG pair that connect to an RTPLAN, then upload it.  Only upload the RTPLAN as necessary.
    */
  private def connectWithPlanViaReg(ct: Series): Option[UploadSet] = {
    if (ct.isModality(ModalityEnum.CT)) {

      // Get the REG file that has the same frame of reference as the image file and references the image series.
      val regOpt = Series.getRegByRegFrameOfReference(ct.FrameOfReferenceUID.get).headOption //  .filter(regRefsImage).headOption

      val procedureOfSeries = PatientProcedure.getProcedureOfSeries(ct)

      if (regOpt.isDefined) {
        val reg = regOpt.get
        val localPlan = Series.getRtplanByFrameOfReference(reg.FrameOfReferenceUID.get) // if there is a copy of the plan in <code>Series</code>
        val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, reg.FrameOfReferenceUID.get) // if the plan is on the server

        (localPlan, remotePlan) match {
          case (Some(rtplan), _) if procedureOfSeries.isDefined => Some(UploadSet(procedureOfSeries.get, ct, Some(reg), Some(rtplan))) // upload CT, REG, and RTPLAN
          case (_, true) if procedureOfSeries.isDefined         => Some(UploadSet(procedureOfSeries.get, ct, Some(reg))) // upload just the CT and REG.  The RTPLAN has already been uploaded
          case _                                                => None
        }
      } else
        None
    } else
      None
  }

  /**
    * Make UploadSet from RTIMAGE series.
    */
  private def uploadableRtimage(rtimage: Series): Option[UploadSet] = {
    if (rtimage.isModality(ModalityEnum.RTIMAGE)) {
      val procedure = PatientProcedure.getProcedureOfSeries(rtimage)
      if (procedure.isDefined) Some(UploadSet(procedure.get, rtimage)) else None
    } else
      None
  }

  /**
    * Given an image series, try to make an upload set.  First try CT with same frame of
    * reference as plan.  If that fails, try CT with REG file with same frame of reference as
    * plan.  If that fails, try RTIMAGE.
    *
    * @param series Image series.
    *
    * @return Either the set of files to upload, or None if the required files can not be found.
    */
  private def seriesToUploadSet(series: Series): Option[UploadSet] = {
    val uploadSet = connectWithPlanByFrameOfRef(series) match {
      case Some(uploadSet) => Some(uploadSet)
      case _ =>
        connectWithPlanViaReg(series) match {
          case Some(uploadSet) => Some(uploadSet)
          case _               => uploadableRtimage(series)
        }
    }
    uploadSet
  }

  /**
    * Look for sets of DICOM series that can be uploaded, and then upload them.
    */
  private def update(): Unit =
    updateSync.synchronized {
      // ignore image series that are too old
      val recent = System.currentTimeMillis - ClientConfig.MaximumDataAge_ms

      // list of all available image series, not have failed before, sorted by acquisition date, and not already sent
      val list = (Series.getByModality(ModalityEnum.CT) ++ Series.getByModality(ModalityEnum.RTIMAGE))
        .filterNot(series => Results.containsSeries(series))
        .filterNot(series => FailedSeries.contains(series.SeriesInstanceUID))
        .sortBy(_.dataDate)
        .filterNot(series => Sent.hasImageSeries(series.SeriesInstanceUID))
        .filter(_.dataDate.getTime > recent)

      val todoList = list.flatMap(series => seriesToUploadSet(series))
      todoList.foreach(uploadSet => upload(uploadSet))
      todoList.map(uploadSet => ConfirmDicomComplete.confirmDicomComplete(uploadSet))
    }

  private val queue = new java.util.concurrent.LinkedBlockingQueue[Boolean]

  private def startUpdateThread(): Unit = {
    class Updater extends Runnable {
      def run(): Unit = {
        while (true) {
          logger.info("Processing new DICOM files.  queue size: " + queue.size)
          update()
          queue.take
        }
      }
    }

    new Thread(new Updater).start()
  }

  /**
    * Indicate that new data is available for processing.  There may or may not be a set that can be
    * processed.  This function sends a message to the Upload thread and returns immediately.
    */
  def scanSeries(): Unit = {
    queue.put(true)
  }

  def init(): Unit = {
    logger.info("initializing Upload")
    startUpdateThread()
    logger.info("Upload has been initialized")
  }

}
