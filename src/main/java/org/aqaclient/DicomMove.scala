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
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil

import java.io.File
import java.util.Date
import scala.annotation.tailrec

/**
  * Utility for getting DICOM via C-MOVE and caching them in the local disk.
  */
object DicomMove extends Logging {

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

      //val sopList: scala.collection.immutable.Seq[String] = alList.map(s => gg(s)).asInstanceOf[scala.collection.immutable.Seq[String]]
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
    * Attempt to get an entire series with one DICOM C-MOVE.
    *
    * This should always work, but it seems that the Varian VMSDBD daemon sometimes only
    * sends a partial list of files.
    */
  private def performCMove(SeriesInstanceUID: String, description: String, PatientID: String, Modality: String): Seq[AttributeList] = {
    val specification = new AttributeList

    val transferDir = makeTransferDir(SeriesInstanceUID, PatientID, Modality)

    def addAttr(tag: AttributeTag, value: String): Unit = {
      val a = AttributeFactory.newAttribute(tag)
      a.addValue(value)
      specification.put(a)
    }

    addAttr(TagByName.QueryRetrieveLevel, "SERIES")
    addAttr(TagByName.SeriesInstanceUID, SeriesInstanceUID)

    ClientUtil.listFiles(transferDir).foreach(ClientUtil.deleteFile) // delete all files in transfer directory
    // Utility.deleteFileTree(dicomReceiver.setSubDir(transferDir.getName))
    dicomReceiver.setSubDir(transferDir.getName)

    val start = System.currentTimeMillis()
    dicomReceiver.cmove(specification, ClientConfig.DICOMSource, ClientConfig.DICOMClient)
    val elapsed = System.currentTimeMillis() - start

    val alList = {
      def seriesMatches(al: AttributeList): Boolean = {
        ClientUtil.getSerUid(al) match {
          case Some(serUid) => serUid.equals(SeriesInstanceUID)
          case _            => false
        }
      }

      val list = ClientUtil.listFiles(transferDir).map(ClientUtil.readDicomFile).filter(_.isRight).map(_.right.get)
      list.filter(seriesMatches)
    }

    val size = alList.size
    val msPerFile = (elapsed.toDouble / size).formatted("%10.3f").trim

    logger.info(s"Successfully performed DICOM C-MOVE   $description  ${transferDir.getName}    Number of files: $size    ms per file: $msPerFile     Elapsed ms: $elapsed")

    alList
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
    * Write the given list of DICOM files to the given directory.
    * @param dir Write to this directory.
    * @param alList Write this DICOM.
    */
  private def writeAlList(dir: File, alList: Seq[AttributeList]): Unit = {
    def writeAl(al: AttributeList): Unit = {
      val sop = al.get(TagByName.SOPInstanceUID).getSingleStringValueOrEmptyString()
      val file = new File(dir, sop + ".dcm")
      DicomUtil.writeAttributeListToFile(al, file, "AQAClient")
    }
    alList.foreach(writeAl)
    logger.info(s"Wrote ${alList.size} DICOM files to Series dir ${dir.getAbsolutePath}")
  }

  /**
    * Do a C-MOVE to get files.  If the number of slices received is fewer than expected, then retry.
    * @param retry Number of times that operation has been retried.
    * @param description For reporting progress and errors.
    * @param SeriesInstanceUID Fetch files for this series.
    * @param findSize Expected number of slices (from C-FIND).
    * @return
    */
  @tailrec
  private def getWithRetry(retry: Int, description: String, SeriesInstanceUID: String, PatientID: String, Modality: String, findSize: Int): Option[Series] = {

    logger.info("trying series " + description + "    retry count " + retry)

    if (ClientConfig.DICOMRetryCount >= retry) {
      def moveFunction() = performCMove(SeriesInstanceUID = SeriesInstanceUID, description = description, PatientID = PatientID, Modality = Modality)
      val alList = DicomSemaphore.processInSemaphore(moveFunction, description)
      val moveSize = alList.size

      logger.info(s"Received $moveSize slices for $description")
      val diff = findSize - moveSize
      if (diff <= 0) {
        if (diff == 0) {
          logger.info(s"Successfully got ${alList.size} slices as expected on try $retry")
        } else {
          logger.warn(s"$description C-FIND returned $findSize results but C-MOVE returned more: $moveSize This should never happen.  Proceeding anyway.")
        }
        val seriesDate = DicomUtil.getTimeAndDate(alList.head, TagByName.SeriesDate, TagByName.SeriesTime).get
        val seriesDir = Series.makeSeriesDir(SeriesInstanceUID, PatientID, Modality, seriesDate)
        writeAlList(seriesDir, alList)
        val series = Series.makeSeriesFromDicomFileDir(seriesDir)
        series
      } else {
        logger.warn(s"$description C-MOVE returned only $moveSize files when C-FIND found $findSize")
        logger.info(s"$description DicomMove.get Retry ${1 + ClientConfig.DICOMRetryCount - retry} of C-MOVE")
        Thread.sleep((ClientConfig.DICOMRetryWait_sec * 1000).toLong)
        getWithRetry(retry + 1, description, SeriesInstanceUID, PatientID, Modality, findSize)
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
    val description = s"C-MOVE PatientID: $PatientID    Modality: $Modality    SeriesInstanceUID $SeriesInstanceUID"

    // Get the SOP UID list via C-FIND.
    val findSize = getCredibleSliceList(SeriesInstanceUID).size

    val series: Option[Series] =
      if (findSize == 0) { // if no slices, then never bother again
        failed(s"C-FIND could not find any slices for series.", SeriesInstanceUID, description)
      } else {
        val res = getWithRetry(1, description = description, SeriesInstanceUID = SeriesInstanceUID, PatientID = PatientID, Modality = Modality, findSize)
        res
      }

    series
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
