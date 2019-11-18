package org.aqaclient

import java.util.Date
import edu.umro.ScalaUtil.Util
import java.io.File
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import scala.xml.XML
import scala.xml.Elem
import scala.xml.PrettyPrinter

/**
 * Represent of a processed set of files.
 */
case class Result(uploadDate: Date, url: String, seriesList: Seq[Series]) extends Logging {
  def file = {
    val fileName = FileUtil.replaceInvalidFileNameCharacters(Util.standardFormat(uploadDate) + "_" + seriesList.head.PatientID, '_')
    new File(ClientConfig.resultsDir, fileName)
  }
  def containsSeries(SeriesInstanceUID: String) = seriesList.filter(s => s.SeriesInstanceUID.equals(SeriesInstanceUID)).nonEmpty
  def containsPlanWithFrameOfReferenceUID(FrameOfReferenceUID: String) = seriesList.filter(s => s.isRtplan && (s.FrameOfReferenceUID.get.equals(FrameOfReferenceUID))).nonEmpty

  def toXml = {
    <Result>
      <uploadDate>{ Util.dateToText(uploadDate) }</uploadDate>
      <url>{ url }</url>
      <SeriesList>
        { seriesList.map(s => s.toXml) }
      </SeriesList>
    </Result>
  }

  /**
   * Return true if at least one of the series references an active patient.
   */
  def referencesActivePatient: Boolean = {
    seriesList.map(s => s.PatientID).distinct.intersect(PatientIDList.getPatientIDList).nonEmpty
  }
}

/**
 * Manage the list of results.
 *
 * Results are kept one per file as XML in the results directory.
 */
object Result extends Logging {

  private val prettyPrinter = new PrettyPrinter(1024, 2)

  /**
   * List of results that have been processed.
   */
  private val resultList = scala.collection.mutable.ArrayBuffer[Result]()

  /**
   * Return true if the SeriesInstanceUID is in the results.
   */
  def containsSeries(SeriesInstanceUID: String): Boolean = resultList.synchronized({
    resultList.filter(r => r.containsSeries(SeriesInstanceUID)).nonEmpty
  })

  /**
   * Return true if there is an RTPLAN with the given FrameOfReferenceUID is in the results.
   */
  def containsPlanWithFrameOfReferenceUID(FrameOfReferenceUID: String): Boolean = resultList.synchronized({
    resultList.filter(r => r.containsPlanWithFrameOfReferenceUID(FrameOfReferenceUID)).nonEmpty
  })

  /**
   * Put a new result into the list and save it to disk.
   */
  def add(result: Result): Unit = {
    resultList.synchronized(resultList += result)
    try {
      prettyPrinter.format(result.toXml)
    } catch {
      case t: Throwable => logger.warn("Unable to save result : " + fmtEx(t))
    }
  }

  /**
   * Read a result from its file and put it in the internal resultList.
   */
  private def reinstateResult(file: File): Unit = {
    try {
      val xml = XML.loadFile(file)
      val date = Util.textToDate((xml \ "date").head.text)
      val url = (xml \ "url").head.text
      val seriesList = (xml \ "SeriesList" \ "Series").map(s => new Series(s.asInstanceOf[Elem]))
      val result = new Result(date, url, seriesList)
      if (result.referencesActivePatient)
        resultList.synchronized(resultList += result)
      else {
        logger.info("Removing result that does not refernce an active patient " + result.seriesList.map(s => s.PatientID).distinct)
        file.delete
      }
    } catch {
      case t: Throwable => logger.warn("Ignoring error while attempting to read saved result from " + file.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  /**
   * Given a directory that contains the result files, reinstate all the results.
   */
  private def reinststateResults = {
    ClientConfig.resultsDir.listFiles.toList.map(file => reinstateResult(file))
    logger.info("Reinstated results list.  Number of results: " + resultList.size)
  }

  def init = {
    reinststateResults
  }

  //  def main(args: Array[String]): Unit = {
  //
  //  }

}