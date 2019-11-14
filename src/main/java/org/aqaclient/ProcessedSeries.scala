package org.aqaclient

import java.util.Date
import edu.umro.ScalaUtil.Logging
import scala.xml.Node
import edu.umro.ScalaUtil.Util
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import java.io.FileWriter
import java.io.File
import scala.xml.PrettyPrinter
import scala.xml.XML

//import java.util.Date
//import scala.xml.Node
//import java.io.File
//import scala.xml.XML
//import com.pixelmed.dicom.AttributeList
//import com.pixelmed.dicom.TagFromName
//import com.pixelmed.dicom.AttributeTag
//import edu.umro.ScalaUtil.DicomUtil
//import java.io.FileWriter
//import scala.xml.PrettyPrinter

case class ProcessedSeries(SeriesInstanceUID: String, PatientID: String, Modality: String, dataDate: Option[Date], ProcessDate: Date) extends Logging {

  def this(node: Node) = this(
    ClientUtil.getAttr(node, "SeriesInstanceUID"),
    ClientUtil.getAttr(node, "PatientID"),
    ClientUtil.getAttr(node, "Modality"),
    ProcessedSeries.optTextToDate(node),
    edu.umro.ScalaUtil.Util.textToDate(ClientUtil.getAttr(node, "ProcessDate")))

  def toXml = {
    val dataDateText = if (dataDate.isDefined) Util.standardFormat(dataDate.get) else "unknown"

    <ProcessedSeries SeriesInstanceUID={ SeriesInstanceUID } PatientID={ PatientID } Modality={ Modality } dataDate={ dataDateText } ProcessDate={ Util.standardFormat(ProcessDate) }/>
  }

  def toText = ProcessedSeries.prettyPrinter.format(toXml)

  override def toString = {
    "SeriesInstanceUID : " + SeriesInstanceUID +
      "    PatientID : " + PatientID +
      "    Modality : " + Modality +
      "    dataDate : " + dataDate +
      "    ProcessDate : " + ProcessDate
  }
}

object ProcessedSeries extends Logging {

  private val fileName = "ProcessedSeries.xml"
  private val file = new File(ClientConfig.DataDir, fileName)
  private val prettyPrinter = new PrettyPrinter(1024, 2)

  private def optTextToDate(node: Node): Option[Date] = {
    try {
      Some(edu.umro.ScalaUtil.Util.textToDate(ClientUtil.getAttr(node, "dataDate")))
    } catch {
      case t: Throwable => None
    }
  }

  private val ProcessedSeriesMap = scala.collection.mutable.HashMap[String, ProcessedSeries]()

  /**
   * Get the ProcessedSeries with the given SeriesInstanceUID.
   */
  def get(SeriesInstanceUID: String): Option[ProcessedSeries] = ProcessedSeriesMap.synchronized(ProcessedSeriesMap.get(SeriesInstanceUID))

  def contains(SeriesInstanceUID: String) = get(SeriesInstanceUID).isDefined

  /**
   * Construct a ProcessedSeries from an AttributeList.
   */
  private def constructProcessedSeries(al: AttributeList): ProcessedSeries = {

    val SeriesInstanceUID = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString
    val PatientID = al.get(TagFromName.PatientID).getSingleStringValueOrEmptyString
    val Modality = al.get(TagFromName.Modality).getSingleStringValueOrEmptyString

    new ProcessedSeries(SeriesInstanceUID, PatientID, Modality, ClientUtil.dataDateTime(al), new Date)
  }

  def add(processedSeries: ProcessedSeries): Unit = {
    ProcessedSeriesMap.synchronized({
      ProcessedSeriesMap += ((processedSeries.SeriesInstanceUID, processedSeries))
      val fw = new FileWriter(file, true) //the true will append the new data
      fw.write(processedSeries.toText) //appends the string to the file
      fw.close
    })
  }

  /**
   * Read the list of series that have already been processed.
   */
  private def readProcessedSeries: Unit = {
    if (file.canRead) {
      val xmlText = "<ProcessedSeriesList>" + ClientUtil.readDicomFile(file) + "<ProcessedSeriesList/>"
      val doc = XML.loadString(xmlText)
      ProcessedSeriesMap.synchronized(
        (doc \ "ProcessedSeries").map(node => new ProcessedSeries(node)).map(ps => ProcessedSeriesMap += ((ps.SeriesInstanceUID, ps))))
      logger.info("Read " + ProcessedSeriesMap.size + " series entries from " + file.getAbsolutePath)
    } else {
      file.getParentFile.mkdirs
      file.createNewFile
    }

  }

  def init = {
    readProcessedSeries

  }
}
