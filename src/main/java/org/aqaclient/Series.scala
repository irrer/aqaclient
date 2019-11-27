package org.aqaclient

import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.Logging
import scala.xml.Elem
import edu.umro.ScalaUtil.Util
import java.util.Date
import edu.umro.ScalaUtil.DicomUtil
import scala.xml.Node
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace

/**
 * Describe a series whose DICOM has been retrieved but has not been processed.
 */
case class Series(
  SeriesInstanceUID: String,
  PatientID: String,
  dataDate: Option[Date],
  Modality: ModalityEnum.Value,
  FrameOfReferenceUID: Option[String],
  RegFrameOfReferenceUID: Option[String]) extends Logging {

  def this(al: AttributeList) = this(
    Series.getString(al, TagFromName.SeriesInstanceUID),
    Series.getString(al, TagFromName.PatientID),
    ClientUtil.dataDateTime(al),
    ModalityEnum.toModalityEnum(Series.getString(al, TagFromName.Modality)),
    Series.getFrameOfReferenceUID(al),
    Series.getRegFrameOfReferenceUID(al))

  def this(xml: Elem) = this(
    (xml \ "@SeriesInstanceUID").head.text,
    (xml \ "@PatientID").head.text,
    Series.optDate((xml \ "@dataDate").headOption),
    ModalityEnum.toModalityEnum((xml \ "@Modality").head.text),
    Series.optText(xml, "FrameOfReferenceUID"),
    Series.optText(xml, "RegFrameOfReferenceUID"))

  val dir = new File(ClientConfig.tmpDir, SeriesInstanceUID)

  private def dateToText(date: Option[Date]) = if (date.isDefined) Util.standardFormat(date.get) else "unknown"

  def toXml = {
    <Series Modality={ Modality.toString } PatientID={ PatientID } dataDate={ dateToText(dataDate) }>
      <SeriesInstanceUID>{ SeriesInstanceUID }</SeriesInstanceUID>
      { if (FrameOfReferenceUID.isDefined) <FrameOfReferenceUID>{ FrameOfReferenceUID }</FrameOfReferenceUID> }
      { if (RegFrameOfReferenceUID.isDefined) <RegFrameOfReferenceUID>{ RegFrameOfReferenceUID }</RegFrameOfReferenceUID> }
    </Series>
  }

  def isModality(modality: ModalityEnum.Value): Boolean = modality.toString.equalsIgnoreCase(Modality.toString)

  def isRtplan = Modality.toString.equals(ModalityEnum.RTPLAN.toString)

  override def toString: String = {
    val dateText = if (dataDate.isDefined) dataDate.get.toString else "None"
    "PatientID: " + PatientID + " : " + Modality + "    date: " + dateText + "    SeriesUID: " + SeriesInstanceUID
  }
}

object Series extends Logging {
  def getString(al: AttributeList, tag: AttributeTag) = al.get(tag).getStringValues.head

  private def optDate(node: Option[Node]) = {
    try {
      Some(Util.textToDate(node.head.text))
    } catch {
      case t: Throwable => None
    }
  }

  private def optText(xml: Elem, tag: String): Option[String] = {
    (xml \ tag).headOption match {
      case Some(node) => Some(node.text)
      case _ => None
    }
  }

  private def getFrameOfReferenceUID(al: AttributeList): Option[String] = {
    val a = al.get(TagFromName.FrameOfReferenceUID)
    if (a == null)
      None
    else
      Some(a.getSingleStringValueOrEmptyString)
  }

  private def getRegFrameOfReferenceUID(al: AttributeList): Option[String] = {

    val FrameOfReferenceUID = getFrameOfReferenceUID(al)

    if (FrameOfReferenceUID.isDefined) {
      DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID).
        map(a => a.getSingleStringValueOrEmptyString).
        distinct.
        filterNot(frmRef => frmRef.equals(FrameOfReferenceUID.get)).
        headOption
    } else
      None
  }

  /**
   * Pool of series whose DICOM contents have been fetched but have not yet been processed.
   */
  private val SeriesPool = scala.collection.mutable.HashMap[String, Series]()

  def get(SeriesInstanceUID: String): Option[Series] = SeriesPool.synchronized({
    SeriesPool.get(SeriesInstanceUID)
  })

  def getAllSeries: List[Series] = SeriesPool.synchronized {
    SeriesPool.values.toList
  }

  /**
   * Get the number of series in pool.
   */
  def size: Int = SeriesPool.synchronized { SeriesPool.size }

  def contains(SeriesInstanceUID: String) = get(SeriesInstanceUID).isDefined

  def getByModality(modality: ModalityEnum.Value): List[Series] = SeriesPool.synchronized({
    SeriesPool.values.filter(s => s.isModality(modality)).toList.sortBy(s => s.dataDate)
  })

  def getRtplanByFrameOfReference(FrameOfReferenceUID: String): Option[Series] = {
    getByModality(ModalityEnum.RTPLAN).filter(s => s.FrameOfReferenceUID.isDefined && s.FrameOfReferenceUID.get.equals(FrameOfReferenceUID)).headOption
  }

  def getRegByFrameOfReference(FrameOfReferenceUID: String): Option[Series] = {
    getByModality(ModalityEnum.REG).filter(s => s.FrameOfReferenceUID.isDefined && s.FrameOfReferenceUID.get.equals(FrameOfReferenceUID)).headOption
  }

  /**
   * Put a series into the pool for uploading.  Also notify the uploader to update.
   */
  def put(series: Series) = {
    SeriesPool.synchronized(SeriesPool.put(series.SeriesInstanceUID, series))
    Trace.trace("put series: " + series)
    Upload.scanSeries
  }

  /**
   * Remove a series.
   */
  def remove(series: Series): Unit = SeriesPool.synchronized {
    if (SeriesPool.get(series.SeriesInstanceUID).isDefined) {
      logger.info("Removing local copy of series " + series)
      SeriesPool -= series.SeriesInstanceUID
    }
    try {
      if (series.dir.exists)
        Utility.deleteFileTree(series.dir)
    } catch {
      case t: Throwable => logger.warn("Unexpected exception while removing Series: " + fmtEx(t))
    }
  }

  /**
   * Given a directory that contains the DICOM files of a series, reinstate the Series (the XML metadata, not the DICOM).
   */
  private def reinstate(dir: File) = {
    try {
      val al = ClientUtil.readDicomFile(dir.listFiles.head).right.get
      val series = new Series(al)
      put(series)
    } catch {
      case t: Throwable => logger.warn("Unexpected error while reading previously saved series from " + dir.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  private def reinststatePreviouslyFetechedSeries = {
    ClientConfig.tmpDir.listFiles.toList.filter(d => d.isDirectory).map(dir => reinstate(dir))
  }

  def init = {
    reinststatePreviouslyFetechedSeries
    logger.info("Series initialization complete.   Number of series in pool: " + Series.size)
  }
}