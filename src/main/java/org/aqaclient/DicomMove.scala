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

import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.Logging
import edu.umro.util.Utility
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.dicomCMove.CMoveResult
import edu.umro.ScalaUtil.dicomCMove.DicomCMoveGetter
import edu.umro.ScalaUtil.dicomCMove.DicomCMoveReceiver

import java.io.File
import scala.annotation.tailrec

/**
  * Utility for getting DICOM via C-MOVE and caching them in the local disk.
  */
object DicomMove extends Logging {

  /** Name of parent dir that contains subdirectories used for DICOM C-MOVEs. */
  private val transferParentDirName = "transferDicomMove"

  /** Parent dir that contains subdirectories used for DICOM C-MOVEs. */
  private val transferParentDir = new File(ClientConfig.seriesDir, transferParentDirName)

  private var dicomCMoveReceiver: Option[DicomCMoveReceiver] = None

  private def getDicomCMoveReceiver: DicomCMoveReceiver = {
    if (dicomCMoveReceiver.isEmpty) {
      transferParentDir.mkdirs()
      dicomCMoveReceiver = Some(DicomCMoveReceiver(transferParentDir, ClientConfig.DICOMClient))
    }
    dicomCMoveReceiver.get
  }

  private var dicomCMoveGetter: Option[DicomCMoveGetter] = None

  private def getDicomCMoveGetter: DicomCMoveGetter = {
    if (dicomCMoveGetter.isEmpty)
      dicomCMoveGetter = Some(new DicomCMoveGetter(ClientConfig.DICOMSource, getDicomCMoveReceiver))
    dicomCMoveGetter.get
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
  private def getCredibleSliceList(SeriesInstanceUID: String, PatientID: String, Modality: String, history: Seq[Seq[String]] = Seq()): Seq[String] = {
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
      val sliceList = DicomFind.getSliceUIDsInSeries(SeriesInstanceUID, PatientID, Modality)
      getCredibleSliceList(SeriesInstanceUID, PatientID, Modality, history :+ sliceList)
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
    * Read the files returned by the C-MOVE.  Only allow those with the correct SeriesInstanceUID.
    * @param result Result from C-MOVE.
    * @param SeriesInstanceUID Only for this series.
    * @param description Description of request, used for reporting errors.
    * @return
    */
  private def readResult(result: CMoveResult, SeriesInstanceUID: String, description: String): Seq[AttributeList] = {
    def readDicom(file: File): Option[AttributeList] = {
      try {
        val al = new AttributeList
        al.read(file)
        val serUid = ClientUtil.getSerUid(al)
        if (serUid.isDefined && serUid.get.equals(SeriesInstanceUID))
          Some(al)
        else
          None
      } catch {
        case t: Throwable =>
          logger.error(s"Unexpected exception reading DICOM files : $description : ${fmtEx(t)}")
          None
      }
    }

    if (result.errorMessage.nonEmpty) {
      logger.error(s"Unable to perform C-MOVE for $description in directory ${result.dir.getAbsolutePath} : ${result.errorMessage.get}")
      Seq()
    } else {
      FileUtil.listFiles(result.dir).flatMap(readDicom)
    }
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

      def close(): Unit = {
        getDicomCMoveGetter.close()
        getDicomCMoveReceiver.close()
      }

      def moveFunction(): Seq[AttributeList] = {
        val result = getDicomCMoveGetter.getSeries(SeriesInstanceUID)
        val list = readResult(result, SeriesInstanceUID, description)
        list
      }

      val alList = DicomSemaphore.processInSemaphore(moveFunction _, close _, description)
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
        Thread.sleep(ClientConfig.DICOMRetryWait_ms)
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
    val findSize = getCredibleSliceList(SeriesInstanceUID, PatientID, Modality).size

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
    val ok = getDicomCMoveReceiver != null
    logger.info("Dicom receiver started: " + ok)
    logger.info("Dicom receiver main dir: " + getDicomCMoveReceiver.mainDir)
  }
}
