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
import scala.xml.Elem

/**
 * Keep track of which series have already been processed by the server, according to the server.
 */
object XProcessedSeries extends Logging {

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

  private val ProcessedSeriesMap = scala.collection.mutable.HashMap[String, Series]()

  /**
   * Get the ProcessedSeries with the given SeriesInstanceUID.
   */
  def get(SeriesInstanceUID: String): Option[Series] = ProcessedSeriesMap.synchronized(ProcessedSeriesMap.get(SeriesInstanceUID))

  def contains(SeriesInstanceUID: String) = get(SeriesInstanceUID).isDefined

  def add(processedSeries: Series): Unit = {
    ProcessedSeriesMap.synchronized({
      ProcessedSeriesMap += ((processedSeries.SeriesInstanceUID, processedSeries))
      val fw = new FileWriter(file, true) //the true will append the new data
      fw.write(prettyPrinter.format(processedSeries.toXml)) //appends the string to the file
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
        (doc \ "ProcessedSeries").map(node => new Series(node.asInstanceOf[Elem])).map(ps => ProcessedSeriesMap += ((ps.SeriesInstanceUID, ps))))
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
