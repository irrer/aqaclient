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

/**
 * Describe a series whose DICOM has been retrieved but has not been processed.
 */
case class Series(
  SeriesInstanceUID: String,
  PatientID: String,
  dataDate: Option[Date],
  Modality: String,
  processedDate: Option[Date],
  FrameOfReferenceUID: Option[String],
  RegFrameOfReferenceUID: Option[String]) extends Logging {

  def this(al: AttributeList) = this(
    Series.getString(al, TagFromName.SeriesInstanceUID),
    Series.getString(al, TagFromName.PatientID),
    ClientUtil.dataDateTime(al),
    Series.getString(al, TagFromName.Modality),
    None,
    Series.getFrameOfReferenceUID(al),
    Series.getRegFrameOfReferenceUID(al))

  def this(xml: Elem) = this(
    (xml \ "@SeriesInstanceUID").head.text,
    (xml \ "@PatientID").head.text,
    Series.optDate((xml \ "@dataDate").headOption),
    (xml \ "@Modality").head.text,
    Series.optDate((xml \ "processedDate").headOption),
    Series.optText(xml, "FrameOfReferenceUID"),
    Series.optText(xml, "RegFrameOfReferenceUID"))

  val dir = new File(ClientConfig.tmpDir, SeriesInstanceUID)

  private def dateToText(date: Option[Date]) = if (date.isDefined) Util.standardFormat(date.get) else "unknown"

  def toXml = {
    <Series Modality={ Modality } PatientID={ PatientID } dataDate={ dateToText(dataDate) }>
      <processedDate>{ dateToText(processedDate) }</processedDate>
      <SeriesInstanceUID>{ SeriesInstanceUID }</SeriesInstanceUID>
      { if (processedDate.isDefined) <processedDate>{ Util.standardFormat(processedDate.get) }</processedDate> }
      { if (FrameOfReferenceUID.isDefined) <FrameOfReferenceUID>{ FrameOfReferenceUID }</FrameOfReferenceUID> }
      { if (RegFrameOfReferenceUID.isDefined) <RegFrameOfReferenceUID>{ RegFrameOfReferenceUID }</RegFrameOfReferenceUID> }
    </Series>
  }

  def toXml(processedDate: Option[Date]): Elem = ???
}

object Series extends Logging {
  def getString(al: AttributeList, tag: AttributeTag) = al.get(tag).getStringValues.head

  def optDate(node: Option[Node]) = {
    try {
      Some(Util.textToDate(node.head.text))
    } catch {
      case t: Throwable => None
    }
  }

  def optText(xml: Elem, tag: String): Option[String] = {
    (xml \ tag).headOption match {
      case Some(node) => Some(node.text)
      case _ => None
    }
  }

  def getFrameOfReferenceUID(al: AttributeList): Option[String] = {
    val a = al.get(TagFromName.FrameOfReferenceUID)
    if (a == null)
      None
    else
      Some(a.getSingleStringValueOrEmptyString)
  }

  def getRegFrameOfReferenceUID(al: AttributeList): Option[String] = {

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

  def contains(SeriesInstanceUID: String) = get(SeriesInstanceUID).isDefined

  def put(series: Series) = SeriesPool.synchronized({
    SeriesPool.put(series.SeriesInstanceUID, series)
  })

  /**
   * Given a directory that contains the DICOM files of a series, reinstate the series.
   */
  private def reinstateSeries(dir: File) = {
    try {
      val al = ClientUtil.readDicomFile(dir.listFiles.head).right.get
      val series = new Series(al)
      put(series)
    } catch {
      case t: Throwable => logger.warn("Unexpected error while reading previously saved series from " + dir.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  private def reinststatePreviouslyFetechedSeries = {
    ClientConfig.tmpDir.listFiles.toList.filter(d => d.isDirectory).map(dir => reinstateSeries(dir))
  }

  def init = {
    reinststatePreviouslyFetechedSeries
  }
}