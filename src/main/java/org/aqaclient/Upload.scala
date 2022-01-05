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

import edu.umro.ScalaUtil.Logging

import java.io.File
import scala.annotation.tailrec

/**
  * Group series into sets of data that can be processed and upload to the AQA platform.
  */

object Upload extends Logging {

  /**  // TODO rm
    * A complete set of DICOM series that can be uploaded.  A set of series may consist of any of these:
    *
    *     - A CT series                 (with or without an RTPLAN)
    *     - A CT series and REG series  (with or without an RTPLAN)
    *     - An RTIMAGE series           (with or without an RTPLAN)
    */
  case class XUploadSet(procedure: Procedure, imageSeries: Series, reg: Option[Series] = None, plan: Option[Series] = None) { // TODO rm
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

  /**
    * Send the zip file to the AQA server. Return true on success.  There may later be a failure on
    * the server side, but this success just indicates that the upload was successful.
    */
  private def uploadToAQA(uploadSet: UploadSet): Option[String] = {
    try {
      val start = System.currentTimeMillis
      logger.info("Starting upload of data set to AQA for procedure " + uploadSet.procedure + " " + uploadSet.description)

      // upload the files and wait for the processing to finish.  Do this in a synchronized so that no
      // other HTTP activity from this service is being attempted while it waits.
      val result = ClientUtil.httpsPost(uploadSet.procedure.URL, uploadSet.zipFile)

      val elapsed = System.currentTimeMillis - start
      result match {
        case Right(good) =>
          logger.info("Elapsed time in ms of upload: " + elapsed + "  Successful upload of data set to AQA for " + uploadSet)
          println(good)
          None
        case Left(failure) =>
          logger.warn("Elapsed time in ms of upload: " + elapsed + "  Failed to upload data set to AQA for " + uploadSet + " : " + fmtEx(failure))
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
        logger.info("Beginning upload of " + uploadSet)
        val msg = uploadToAQA(uploadSet)
        val ok = msg.isEmpty
        if (ok)
          logger.info("Successfully uploaded " + uploadSet)
        else {
          logger.warn("Failure while uploading " + uploadSet + " : " + msg.get)
        }

        logger.info("Executing post-processing...")
        uploadSet.postProcess(msg)
        logger.info("Finished executing post-processing.")
        try { uploadSet.zipFile.delete() }
        catch { case _: Throwable => }
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

  private val queue = new java.util.concurrent.LinkedBlockingQueue[UploadSet]

  private def startUpdateThread(): Unit = {
    class Updater extends Runnable {
      def run(): Unit = {
        while (true) {
          val uploadSet = queue.take
          logger.info("Processing new upload " + uploadSet)
          upload(uploadSet)
        }
      }
    }

    new Thread(new Updater).start()
  }

  /**
    * Put an upload set on the FIFO queue to be sent to the server.
    *
    * @param uploadSet Data to upload.
    */
  def put(uploadSet: UploadSet): Unit = {
    queue.put(uploadSet)
  }

  def init(): Unit = {
    logger.info("initializing Upload")
    startUpdateThread()
    logger.info("Upload has been initialized")
  }

}
