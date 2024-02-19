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

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.network.ReceivedObjectHandler
import edu.umro.ScalaUtil.DicomCFind
import edu.umro.ScalaUtil.DicomReceiver
import edu.umro.ScalaUtil.Logging
import edu.umro.util.Utility
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.FileUtil

import java.io.File
import java.util.Date
import java.util.concurrent.Semaphore
import scala.annotation.tailrec

/**
  * Utility for getting DICOM via C-MOVE and caching them in the local disk.
  */
object DicomMove extends Logging {

  /** Used to limit use of DICOM C-MOVEs and C-FINDs to one. */
  val dicomSemaphore = new Semaphore(1)

  /** Name of parent dir that contains subdirectories used for DICOM C-MOVEs. */
  private val transferParentDirName = "transferDicomMove"

  /** Parent dir that contains subdirectories used for DICOM C-MOVEs. */
  private val transferParentDir = new File(ClientConfig.seriesDir, transferParentDirName)

  /**
    * Create a directory for the C-MOVE of a single DICOM series.  The directory will
    * only live until it is moved to its final resting place.
    *
    * @param SeriesInstanceUID Series to get.
    * @param PatientID Patient ID to make good name for dir.
    * @param Modality DICOM modality to make good name for dir.
    * @return New directory (created).
    */
  @tailrec
  private def makeTransferDir(SeriesInstanceUID: String, PatientID: String, Modality: String): File = {
    val name = {
      val t = ClientUtil.timeAsFileNameFormat.format(new Date) + "_" + PatientID + "_" + Modality + "_" + SeriesInstanceUID
      FileUtil.replaceInvalidFileNameCharacters(t, '_').replace(' ', '_').replaceAll("_+", "_")
    }

    val dir = new File(transferParentDir, name)
    if (dir.isDirectory) {
      logger.warn("Unexpected condition where temporary directory already exists, but handling it: " + dir.getAbsolutePath)
      Thread.sleep(100)
      makeTransferDir(SeriesInstanceUID, PatientID, Modality)
    } else {
      dir.mkdir()
      logger.info("Created temporary dir: " + dir.getAbsolutePath)
      dir
    }
  }

  private class MyReceivedObjectHandler extends ReceivedObjectHandler {
    override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String): Unit = {
      logger.info("Received file " + fileName)
    }
  }

  private lazy val dicomReceiver = {
    logger.info("Starting DicomReceiver ...")
    val dr = new DicomReceiver(transferParentDir, ClientConfig.DICOMClient, new MyReceivedObjectHandler)
    logger.info("Started DicomReceiver.  This DICOM connection: " + ClientConfig.DICOMClient)
    dr
  }

  /**
    * Copy files from the transfer directory to the series directory.  Use those files to
    * create a Series object.  When done, delete the transfer directory.
    *
    * @param transferDir Temporary place to put files.
    * @return Series, if everything goes right.
    */
  private def moveTransferDirToSeriesDir(transferDir: File): Option[Series] = {
    val transferList = ClientUtil.listFiles(transferDir)
    if (transferList.isEmpty) None
    else
      try {
        val alList = transferList.map(f => ClientUtil.readDicomFile(f)).filter(al => al.isRight).map(al => al.right.get)
        val seriesDir = Series.dirOf(alList)
        Utility.deleteFileTree(seriesDir)
        seriesDir.getParentFile.mkdirs
        transferDir.renameTo(seriesDir)
        val series = Series.makeSeriesFromDicomFileDir(seriesDir)
        series
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected error while moving files in transfer directory: " + fmtEx(t))
          None
      }
  }

  /**
    * Get a list of the SOPInstanceUIDs of the series via C-FIND
    */
  private def getSliceList(SeriesInstanceUID: String): Seq[String] = {
    try {
      val al = new AttributeList
      val ser = AttributeFactory.newAttribute(TagByName.SeriesInstanceUID)
      ser.addValue(SeriesInstanceUID)
      al.put(ser)
      val sop = AttributeFactory.newAttribute(TagByName.SOPInstanceUID)
      al.put(sop)

      val alList = DicomCFind.cfind(
        ClientConfig.DICOMClient.aeTitle,
        ClientConfig.DICOMSource,
        al,
        DicomCFind.QueryRetrieveLevel.IMAGE,
        None,
        DicomCFind.QueryRetrieveInformationModel.StudyRoot
      )

      def gg(al: AttributeList) = {
        val s = al.get(TagByName.SOPInstanceUID).getSingleStringValueOrEmptyString
        s
      }

      val sopList = alList.map(s => gg(s))
      logger.info("SeriesSeriesInstanceUID C-FIND found " + sopList.size + " slices for SeriesInstanceUID " + SeriesInstanceUID)
      sopList
    } catch {
      case t: Throwable =>
        logger.error("Could not get list of slices for Series UID " + SeriesInstanceUID + " : " + fmtEx(t))
        Seq[String]()
    }
  }

  /**
    * Get the SOPInstanceUID of a file.
    */
  private def fileToSopInstanceUID(file: File): Option[String] = {
    try {
      val al = new AttributeList
      al.read(file)
      al.get(TagByName.SOPInstanceUID).getSingleStringValueOrEmptyString match {
        case ""  => None
        case uid => Some(uid)
      }

    } catch {
      case t: Throwable =>
        logger.warn("Unable to get SOPInstanceUID from file " + file.getAbsolutePath + " : " + fmtEx(t))
        None
    }
  }

  /**
    * Get the list of all SOPInstanceUID in the transfer directory.
    */
  private def getSopList(transferDir: File): Seq[String] = ClientUtil.listFiles(transferDir).flatMap(f => fileToSopInstanceUID(f))

  /**
    * Attempt to get an entire series with one DICOM C-MOVE.
    *
    * This should always work, but it seems that the Varian VMSDBD daemon sometimes only
    * sends a partial list of files.
    */
  private def getEntireSeries(SeriesInstanceUID: String, transferDir: File): Seq[String] = {
    val specification = new AttributeList

    def addAttr(tag: AttributeTag, value: String): Unit = {
      val a = AttributeFactory.newAttribute(tag)
      a.addValue(value)
      specification.put(a)
    }

    addAttr(TagByName.QueryRetrieveLevel, "SERIES")
    addAttr(TagByName.SeriesInstanceUID, SeriesInstanceUID)

    ClientUtil.listFiles(transferDir).map(f => f.delete) // delete all files in transfer directory
    Utility.deleteFileTree(dicomReceiver.setSubDir(transferDir.getName))

    val didAcquire = dicomSemaphore.tryAcquire(ClientConfig.DicomTimeout_ms, java.util.concurrent.TimeUnit.MILLISECONDS)
    if (!didAcquire)
      logger.error("Could not acquire DICOM semaphore.  Proceeding with C-MOVE anyway.")
    try {
      val start = System.currentTimeMillis()
      dicomReceiver.cmove(specification, ClientConfig.DICOMSource, ClientConfig.DICOMClient)
      logger.info("Successfully copied DICOM files.")
      val elapsed = System.currentTimeMillis() - start

      val size = ClientUtil.listFiles(transferDir).size
      val msPerFile = (elapsed.toDouble / size).formatted("%10.3f").trim

      logger.info("Successfully performed DICOM C-MOVE to transfer dir " + transferDir.getName + "  Number of files: " + size + "    ms per file: " + msPerFile + "    Elapsed ms: " + elapsed)
    } catch {
      case t: Throwable => logger.error("Unexpected exception during DICOM C-MOVE: " + fmtEx(t))
    } finally {
      dicomSemaphore.release()
    }

    getSopList(transferDir)
  }

  /**
    * Perform a C-FIND multiple times and require the maximum slice count multiple times before it
    * is considered credible.
    *
    * @param SeriesInstanceUID Get slice list for this series.
    * @param history List of slice lists gotten on previous attempts, in chronological order.
    * @return List of slices that seems to be final (no more coming).
    */
  @tailrec
  private def getCredibleSliceList(SeriesInstanceUID: String, history: Seq[Seq[String]] = Seq()): Seq[String] = {
    // At least this many C-FINDS must return the same result before we believe it.
    val minAttempts = 3

    // Wait this many ms between C-FINDs to allow time for more slices to arrive.  Also avoids overloading the server.
    val cFindWaitInterval_ms = 500

    val latest = history.takeRight(minAttempts).map(_.size)
    // of the last tries, there must be a minimum number of tries that are all the same size.
    val isCredible = (latest.size == minAttempts) && (latest.distinct.size == 1)

    if (isCredible) {
      logger.info("getCredibleSliceList: C-FIND was executed " + history.size + " times to get a consistent list of " + history.last.size + " slices   " + history.size + " times.")
      history.last
    } else {
      if (history.nonEmpty) Thread.sleep(cFindWaitInterval_ms)
      val sliceList = getSliceList(SeriesInstanceUID)
      getCredibleSliceList(SeriesInstanceUID, history :+ sliceList)
    }
  }

  /**
    * Log a message, marked the series as failed (so it will not be tried in the future), and return None.  Note
    * that the list of series marked as failed will be reset when the server restarts, at which point they will
    * be tried again.
    */
  private def failed(msg: String, SeriesInstanceUID: String, description: String) = {
    logger.warn(description + " " + msg)
    FailedSeries.put(SeriesInstanceUID)
    None
  }

  /**
    * Do a C-MOVE to get files.  If the number of slices received is fewer than expected, then retry.
    * @param retry Number of times that operation has been retried.
    * @param description For reporting progress and errors.
    * @param transferDir Put DICOM files here.
    * @param SeriesInstanceUID Fetch files for this series.
    * @param findSize Expected number of slices (from C-FIND).
    * @return
    */
  @tailrec
  private def getAll(retry: Int, description: String, transferDir: File, SeriesInstanceUID: String, findSize: Int): Option[Series] = {

    logger.warn("trying series " + description + "    retry count " + retry)

    if (ClientConfig.DICOMRetryCount >= retry) {
      val sopCMoveList = getEntireSeries(SeriesInstanceUID, transferDir)
      val moveSize = sopCMoveList.size

      logger.info(s"Received $moveSize slices for $description")
      val diff = findSize - moveSize
      if (diff <= 0) {
        if (diff == 0) {
          logger.info(s"Successfully got ${sopCMoveList.size} slices as expected on try $retry")
        } else {
          logger.warn(s"$description C-FIND returned $findSize results but C-MOVE returned more: $moveSize This should never happen.  Proceeding anyway.")
        }
        val series = moveTransferDirToSeriesDir(transferDir)
        series
      } else {
        logger.warn(s"$description C-MOVE returned only $moveSize files when C-FIND found $findSize")
        logger.info(s"$description DicomMove.get Retry ${1 + ClientConfig.DICOMRetryCount - retry} of C-MOVE")
        Thread.sleep((ClientConfig.DICOMRetryWait_sec * 1000).toLong)
        getAll(retry + 1, description, transferDir, SeriesInstanceUID, findSize)
      }
    } else {
      val msg = s"Giving up on getting series $SeriesInstanceUID via C-MOVE after retrying ${ClientConfig.DICOMRetryCount} times.  It will be ignored until this service restarts."
      failed(msg, SeriesInstanceUID, description)
    }
  }

  /**
    * Get all files for the given series.  On failure return None and log an error message.
    *
    * @param SeriesInstanceUID : Get this series
    * @param PatientID         : patient ID used to log descriptive messages
    * @param Modality          : Modality used to log descriptive messages
    *
    * @return Get a DICOM series.
    */
  def get(SeriesInstanceUID: String, PatientID: String, Modality: String): Option[Series] = {
    val transferDir = makeTransferDir(SeriesInstanceUID, PatientID, Modality)
    val description = "Series: " + SeriesInstanceUID + " : " + PatientID + " : " + Modality

    // Get the SOP UID list via C-FIND.
    val sopCFindList = getCredibleSliceList(SeriesInstanceUID)
    val findSize = sopCFindList.size

    val result: Option[Series] = if (sopCFindList.isEmpty) { // if no slices, then never bother again
      failed(s"C-FIND could not find any slices for series.", SeriesInstanceUID, description)
      // make an empty series so that next time it will be ignored (and more quickly).
      val res = Series.makeEmptySeries(SeriesInstanceUID, PatientID, Modality)
      Some(res)
    } else {
      val res = getAll(1, description, transferDir, SeriesInstanceUID, findSize)
      res
    }

    // the transfer directory should be deleted if it still exists
    try {
      Utility.deleteFileTree(transferDir)
    } catch {
      case t: Throwable =>
        logger.warn(s"Ignoring unexpected exception deleting transfer directory for $description : ${fmtEx(t)}")
    }

    result
  }

  /**
    * Remove temporary files if there are any.
    */
  private def cleanup = {
    if (transferParentDir.exists)
      try {
        Utility.deleteFileTree(transferParentDir)
      } catch {
        case _: Throwable => ;
      }

    ClientConfig.seriesDir.listFiles.filter(f => f.getName.toLowerCase.endsWith(".tmp")).map(f => f.delete)
  }

  /**
    * Initialize by starting the DICOM receiver, but do not fetch any data.
    */
  def init(): Unit = {
    logger.info("initializing DicomMove")
    cleanup
    transferParentDir.mkdirs
    val ok = dicomReceiver != null
    logger.info("Dicom receiver started: " + ok)
    logger.info("Dicom receiver main dir: " + dicomReceiver.mainDirName)
  }
}
