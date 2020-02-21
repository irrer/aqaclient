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
   * Get all files for the given series.  On failure return an empty list and log an error message.
   */
  def get(SeriesInstanceUID: String): Option[Series] = activeDirName.synchronized({
    clearActiveDir
    val specification = new AttributeList

    def addAttr(tag: AttributeTag, value: String): Unit = {
      val a = AttributeFactory.newAttribute(tag)
      a.addValue(value)
      specification.put(a)
    }

    addAttr(TagFromName.QueryRetrieveLevel, "SERIES")
    addAttr(TagFromName.SeriesInstanceUID, SeriesInstanceUID)

    val series: Option[Series] = dicomReceiver.cmove(specification, ClientConfig.DICOMSource, ClientConfig.DICOMClient, SOPClass.PatientRootQueryRetrieveInformationModelMove) match {
      case Some(msg) => {
        logger.error("C-MOVE failed: " + msg)
        None
      }
      case _ => moveActiveDirToSeriesDir
    }

    // the active directory should be deleted if it still exists
    if (activeDir.exists)
      Utility.deleteFileTree(activeDir)

    series
  })

  /**
   * Initialize by starting the DICOM receiver, but do not fetch any data.
   */
  def init = {
    logger.info("initializing DicomMove")
    dicomReceiver.mainDirName
  }
}
