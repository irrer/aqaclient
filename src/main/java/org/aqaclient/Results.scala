package org.aqaclient

import java.util.Date
import edu.umro.ScalaUtil.Util
import java.io.File
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import scala.xml.XML
import scala.xml.Elem
import scala.xml.PrettyPrinter
import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.Trace
import java.io.ByteArrayOutputStream
import scala.xml.Node
import scala.xml.NodeSeq

/**
 * Manage and cache the list of results.
 *
 * Results are retrieved from the AQA server on a per-patient basis.
 *
 * Each set is the list of results for that patient.
 */
object Results extends Logging {

  private val prettyPrinter = new PrettyPrinter(1024, 2)

  /**
   * List of results that have been processed.
   */
  private val resultList = scala.collection.mutable.HashMap[String, Elem]()

  private def updatePatient(patientId: String): Elem = {
    val url = ClientConfig.AQAURL + "/GetSeries?PatientID=" + patientId
    val elem = HttpsClient.httpsGet(url, ClientConfig.AQAUser, ClientConfig.AQAPassword) match {
      case Left(exception) => {
        logger.warn("Unable to fetch result list for patient " + patientId + " : " + fmtEx(exception))
        <SeriesList></SeriesList>
      }
      case Right(representation) => {
        val outStream = new ByteArrayOutputStream
        representation.write(outStream)
        val e = XML.loadString(outStream.toString)
        logger.info("Retrieved " + (e \ "Series").size + " results for patient " + patientId)
        Trace.trace("\n\nseries list:\n" + (new scala.xml.PrettyPrinter(1024, 2)).format(e) + "\n\n")
        e
      }
    }
    resultList.put(patientId, elem)
    elem
  }

  private def getPatientResultList(patientId: String): Elem = resultList.synchronized {
    resultList.get(patientId) match {
      case Some(elem) => elem
      case _ => updatePatient(patientId)
    }
  }

  /**
   * Mark the given patient's information as stale by removing it from the list.  If the patient's data is
   * needed, then a fresh, updated copy will be retrieved.  This function should
   * be called when a new data set is uploaded for analysis.
   */
  def markAsStale(patientId: String): Unit = resultList.synchronized {
    resultList -= patientId
  }

  def getProcedureOfSeries(patientId: String, SeriesInstanceUID: String): Option[Procedure] = {
    (getPatientResultList(patientId) \ "Series" \ "SeriesInstanceUID").find(n => n.head.text.equals(SeriesInstanceUID)) match {
      case Some(node) => {
        val proc = (node \ "Procedure").headOption
        if (proc.isDefined)
          Procedure.getProcedure(proc.get.text)
        else
          None
      }
      case _ => None
    }
  }

  /**
   * Return true if the SeriesInstanceUID is in the results.
   */
  def containsSeries(patientId: String, SeriesInstanceUID: String): Boolean = {
    (getPatientResultList(patientId) \ "Series" \ "SeriesInstanceUID").find(n => n.head.text.equals(SeriesInstanceUID)).nonEmpty
  }

  /**
   * Return true if there is an RTPLAN with the given FrameOfReferenceUID is in the results.
   */
  def containsPlanWithFrameOfReferenceUID(patientId: String, FrameOfReferenceUID: String): Boolean = {
    (getPatientResultList(patientId) \ "Series").find(n => {
      (n \ "Modality").head.text.equals(ModalityEnum.RTPLAN.toString) &&
        (n \ "FrameOfReferenceUID").head.text.equals(FrameOfReferenceUID)
    }).nonEmpty
  }

  /**
   * Initialize by getting series for all patients from the AQA server.
   */
  def init = {
    val count = PatientIDList.getPatientIDList.map(patId => updatePatient(patId)).size
    logger.info("Retrieved series lists for " + count + " patients from the AQA server.")
  }
}