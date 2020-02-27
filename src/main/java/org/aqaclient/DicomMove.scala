package org.aqaclient

import edu.umro.ScalaUtil.DicomReceiver
import com.pixelmed.network.ReceivedObjectHandler
import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.Logging
import scala.collection.mutable.ArrayBuffer
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.SOPClass
import java.io.File
import edu.umro.util.Utility
import java.util.Date
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import java.text.SimpleDateFormat
import edu.umro.ScalaUtil.DicomCFind
import edu.umro.ScalaUtil.DicomCFind.QueryRetrieveLevel

/**
 * Utility for getting DICOM via C-MOVE and caching them in the local disk.
 */
object DicomMove extends Logging {

  val activeDirName = "active"
  val activeDir = new File(ClientConfig.seriesDir, activeDirName)
  activeDir.mkdirs

  private class MyReceivedObjectHandler extends ReceivedObjectHandler {
    override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String) = {
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
  private def clearActiveDir: Unit = {
    activeDir.mkdirs
    val fileList = ClientUtil.listFiles(activeDir)
    val timeout = System.currentTimeMillis + (10 * 1000)
    while (ClientUtil.listFiles(activeDir).nonEmpty && (System.currentTimeMillis < timeout)) {
      logger.info("Removing " + ClientUtil.listFiles(activeDir).size + " obsolete files from DICOM active directory " + activeDir.getAbsolutePath)
      ClientUtil.listFiles(activeDir).map(f => f.delete)
      Thread.sleep(1000)
    }
  }

  /**
   * Get some reasonable approximation of the date.
   */
  private def dateOfSeries(al: AttributeList): Date = {
    val dateTimeTagPairList = List(
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      (TagFromName.CreationDate, TagFromName.CreationTime),
      (TagFromName.StudyDate, TagFromName.StudyTime),
      (TagFromName.SeriesDate, TagFromName.SeriesTime),
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime))

    def get(dateTag: AttributeTag, timeTag: AttributeTag): Option[Date] = {
      try {
        val d = DicomUtil.dicomDateFormat.parse(al.get(dateTag).getSingleStringValueOrNull)
        val t = {
          val text: String = al.get(timeTag).getSingleStringValueOrNull
          new Date(DicomUtil.parseDicomTime(text).get)
        }

        Some(new Date(d.getTime + t.getTime))

      } catch {
        case t: Throwable => {
          None
        }
      }
    }

    val list = dateTimeTagPairList.map(dt => get(dt._1, dt._2))
    list.flatten.head
  }

  private def moveActiveDirToSeriesDir: Option[Series] = {
    val activeList = ClientUtil.listFiles(activeDir)
    if (activeList.isEmpty) None
    else try {
      val alList = activeList.map(f => ClientUtil.readDicomFile(f)).filter(al => al.isRight).map(al => al.right.get)
      val seriesDir = Series.dirOf(alList)
      Utility.deleteFileTree(seriesDir)
      seriesDir.getParentFile.mkdirs
      activeDir.renameTo(seriesDir)
      val series = new Series(alList.head, seriesDir)
      Some(series)
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error while moving files in active directory: " + fmtEx(t))
        None
      }
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
        al, DicomCFind.QueryRetrieveLevel.IMAGE,
        None,
        DicomCFind.QueryRetrieveInformationModel.StudyRoot)
      val sopList = alList.map(s => s.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString)
      logger.info("SerieSeriesInstanceUID C-FIND found " + sopList.size + " slices for SeriesInstanceUID " + SeriesInstanceUID)
      sopList
    } catch {
      case t: Throwable => {
        logger.error("Could not get list of slices for Series UID " + SeriesInstanceUID + " : " + fmtEx(t))
        Seq[String]()
      }
    }
  }

  /**
   * Get the SOPInstanceUID of a file.
   */
  private def fileToSopInstanceUID(file: File): Option[String] = {
    try {
      val al = new AttributeList
      val a = al.read(file)
      al.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString match {
        case "" => None
        case uid => Some(uid)
      }

    } catch {
      case t: Throwable => {
        logger.warn("Unable to get SOPInstanceUID from file " + file.getAbsolutePath + " : " + fmtEx(t))
        None
      }
    }
  }

  /**
   * Get the list of all SOPInstanceUID in the active directory.
   */
  private def getSopList: Seq[String] = ClientUtil.listFiles(activeDir).map(f => fileToSopInstanceUID(f)).flatten

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
    dicomReceiver.cmove(specification, ClientConfig.DICOMSource, ClientConfig.DICOMClient)
    getSopList
  }

  /**
   * Get all files for the given series.  On failure return None and log an error message.
   */
  def get(SeriesInstanceUID: String): Option[Series] = activeDirName.synchronized({
    clearActiveDir

    // Get the SOP UID list via C-FIND.  Assume that this works.
    val sopCFindList = getSliceList(SeriesInstanceUID)

    def failed(msg: String) = {
      logger.warn(msg)
      FailedSeries.put(SeriesInstanceUID)
      None
    }

    def getAll(retry: Int = ClientConfig.DICOMRetryCount): Option[Series] = {
      val sopCMoveList = getEntireSeries(SeriesInstanceUID)
      (sopCFindList.size - sopCMoveList.size) match {
        case 0 => moveActiveDirToSeriesDir // success!
        case diff if (diff < 0) => failed("C-FIND returned " + sopCFindList.size + " results but C-MOVE returned more: " + sopCMoveList.size + ".  This should never happen.")
        case diff if (diff > 0) => {
          logger.warn("C-MOVE returned only " + sopCMoveList.size + " files when C-FIND found " + sopCFindList.size)
          if (0 >= retry)
            failed("Out of retries for series " + SeriesInstanceUID + " after trying " + ClientConfig.DICOMRetryCount +
              " times.  Giving up on this series.  It will be ignored until this service restarts.")
          else {
            val r = retry - 1
            logger.info("Retry " + (ClientConfig.DICOMRetryCount - r) + " of C-MOVE for series " + SeriesInstanceUID)
            Thread.sleep((ClientConfig.DICOMRetryWait_sec * 1000).toLong)
            getAll(r)
          }
        }
      }
    }

    val result = getAll(ClientConfig.DICOMRetryCount)

    // the active directory should be deleted if it still exists
    if (activeDir.exists) Utility.deleteFileTree(activeDir)

    result
  })

  /**
   * Initialize by starting the DICOM receiver, but do not fetch any data.
   */
  def init = {
    logger.info("initializing DicomMove")
    dicomReceiver.mainDirName
  }
}
