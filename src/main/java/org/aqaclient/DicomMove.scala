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
import com.pixelmed.dicom.TagFromName
import com.pixelmed.network.ReceivedObjectHandler
import edu.umro.ScalaUtil.DicomCFind
import edu.umro.ScalaUtil.DicomReceiver
import edu.umro.ScalaUtil.Logging
import edu.umro.util.Utility

import java.io.File
import scala.annotation.tailrec

/**
  * Utility for getting DICOM via C-MOVE and caching them in the local disk.
  */
object DicomMove extends Logging {

  val activeDirName = "active"
  val activeDir = new File(ClientConfig.seriesDir, activeDirName)
  activeDir.mkdirs

  private class MyReceivedObjectHandler extends ReceivedObjectHandler {
    override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String): Unit = {
      logger.info("Received file " + fileName)
    }
  }

  private lazy val dicomReceiver = {
    logger.info("Starting DicomReceiver ...")
    val dr = new DicomReceiver(ClientConfig.seriesDir, ClientConfig.DICOMClient, new MyReceivedObjectHandler)
    Utility.deleteFileTree(dr.setSubDir(activeDirName))
    logger.info("Started DicomReceiver.  This DICOM connection: " + ClientConfig.DICOMClient)
    dr
  }

  /**
    * Remove files from the active directory.  There should not be any there, but it is
    * possible if the system was shut down while a transfer was taking place.
    */
  private def clearActiveDir(): Unit = {
    activeDir.mkdirs
    val timeout = System.currentTimeMillis + (10 * 1000)
    while (ClientUtil.listFiles(activeDir).nonEmpty && (System.currentTimeMillis < timeout)) {
      logger.info("Removing " + ClientUtil.listFiles(activeDir).size + " obsolete files from DICOM active directory " + activeDir.getAbsolutePath)
      ClientUtil.listFiles(activeDir).map(f => f.delete)
      Thread.sleep(1000)
    }
  }

  private def moveActiveDirToSeriesDir: Option[Series] = {
    val activeList = ClientUtil.listFiles(activeDir)
    if (activeList.isEmpty) None
    else
      try {
        val alList = activeList.map(f => ClientUtil.readDicomFile(f)).filter(al => al.isRight).map(al => al.right.get)
        val seriesDir = Series.dirOf(alList)
        Utility.deleteFileTree(seriesDir)
        seriesDir.getParentFile.mkdirs
        activeDir.renameTo(seriesDir)
        val series = Series.makeSeriesFromDicomFileDir(seriesDir)
        Some(series)
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected error while moving files in active directory: " + fmtEx(t))
          None
      }
  }

  /**
    * Get a list of the SOPInstanceUIDs of the series via C-FIND
    */
  private def getSliceList(SeriesInstanceUID: String): Seq[String] = {
    try {
      val al = new AttributeList
      val ser = AttributeFactory.newAttribute(TagFromName.SeriesInstanceUID)
      ser.addValue(SeriesInstanceUID)
      al.put(ser)
      val sop = AttributeFactory.newAttribute(TagFromName.SOPInstanceUID)
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
        val s = al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString
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
      al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString match {
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
    * Get the list of all SOPInstanceUID in the active directory.
    */
  private def getSopList: Seq[String] = ClientUtil.listFiles(activeDir).flatMap(f => fileToSopInstanceUID(f))

  /**
    * Attempt to get an entire series with one DICOM C-MOVE.
    *
    * This should always work, but it seems that the Varian VMSDBD daemon sometimes only
    * sends a partial list of files.
    */
  private def getEntireSeries(SeriesInstanceUID: String): Seq[String] = {
    val specification = new AttributeList

    def addAttr(tag: AttributeTag, value: String): Unit = {
      val a = AttributeFactory.newAttribute(tag)
      a.addValue(value)
      specification.put(a)
    }

    addAttr(TagFromName.QueryRetrieveLevel, "SERIES")
    addAttr(TagFromName.SeriesInstanceUID, SeriesInstanceUID)

    ClientUtil.listFiles(activeDir).map(f => f.delete) // delete all files in active directory
    ClientConfig.DICOMClient.synchronized(dicomReceiver.cmove(specification, ClientConfig.DICOMSource, ClientConfig.DICOMClient))
    getSopList
  }

  /**
    * Perform a C-FIND multiple times and require the maximum slice count multiple times before it
    * is considered credible.
    */
  @tailrec
  private def getCredibleSliceList(SeriesInstanceUID: String, history: Seq[Seq[String]] = Seq()): Seq[String] = {
    // At least this many C-FINDS must return the same result before we believe it.
    val minCredibleSize = 3

    // Wait this many ms between C-FINDs to avoid overloading the server
    val cFindWaitInterval_ms = 250

    // the maximum number of slices found with multiple attempts
    val maxSliceCount = if (history.isEmpty) 0 else history.maxBy(h => h.size).size

    // list of attempts that have the max number of slices
    val hasMaxSliceCount = history.filter(h => h.size == maxSliceCount)

    if (hasMaxSliceCount.size >= minCredibleSize) {
      logger.info("getCredibleSliceList: C-FIND was executed " + history.size + " times to get a consistent list of " + hasMaxSliceCount.head.size + " slices   " + hasMaxSliceCount.size + " times.")
      hasMaxSliceCount.head
    } else {
      if (history.nonEmpty) Thread.sleep(cFindWaitInterval_ms)
      val sliceList = getSliceList(SeriesInstanceUID)
      getCredibleSliceList(SeriesInstanceUID, history :+ sliceList)
    }
  }

  /**
    * Get all files for the given series.  On failure return None and log an error message.
    *
    * @param SeriesInstanceUID : Get this series
    * @param description       : Text used to log descriptive messages
    */
  def get(SeriesInstanceUID: String, description: String): Option[Series] =
    activeDirName.synchronized({
      clearActiveDir()

      // Get the SOP UID list via C-FIND.
      val sopCFindList = getCredibleSliceList(SeriesInstanceUID)

      /**
        * Log a message, marked the series as failed (so it will not be tried in the future), and return None.  Note
        * that the list of series marked as failed will be reset when the server restarts, at which point they will
        * be tried again.
        */
      def failed(msg: String) = {
        logger.warn(description + " " + msg)
        FailedSeries.put(SeriesInstanceUID)
        None
      }

      @tailrec
      def getAll(retry: Int): Option[Series] = {

        logger.warn("trying series " + description + "    retry count " + retry)

        if (ClientConfig.DICOMRetryCount >= retry) {
          val sopCMoveList = getEntireSeries(SeriesInstanceUID)

          logger.info("Number of slices received for " + description + " " + SeriesInstanceUID + " : " + sopCMoveList.size)
          val diff = sopCFindList.size - sopCMoveList.size
          if (diff <= 0) {
            if (diff == 0) {
              logger.info("Successfully got " + sopCMoveList.size + " slices on try " + retry)
            } else {
              logger.warn(description + "C-FIND returned " + sopCFindList.size + " results but C-MOVE returned more: " + sopCMoveList.size + ".  This should never happen.  Proceeding anyway.")
            }
            moveActiveDirToSeriesDir
          } else {
            logger.warn(description + " C-MOVE returned only " + sopCMoveList.size + " files when C-FIND found " + sopCFindList.size)
            logger.info(description + " DicomMove.get Retry " + (1 + ClientConfig.DICOMRetryCount - retry) + " of C-MOVE for series " + SeriesInstanceUID)
            Thread.sleep((ClientConfig.DICOMRetryWait_sec * 1000).toLong)
            getAll(retry + 1)
          }
        } else {
          failed(
            " Giving up on getting series " + SeriesInstanceUID + " via C-MOVE after retrying " + ClientConfig.DICOMRetryCount +
              " times.    It will be ignored until this service restarts."
          )
        }
      }

      if (sopCFindList.isEmpty) { // if no slices, then never bother again (until next server restart)
        failed("C-FIND could not find any slices for this series: " + SeriesInstanceUID)
      } else {
        val result = getAll(1)

        // the active directory should be deleted if it still exists
        if (activeDir.exists) Utility.deleteFileTree(activeDir)

        result
      }
    })

  /**
    * Initialize by starting the DICOM receiver, but do not fetch any data.
    */
  def init: String = {
    logger.info("initializing DicomMove")
    dicomReceiver.mainDirName
  }
}
