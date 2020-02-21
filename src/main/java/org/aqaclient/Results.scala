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
import org.restlet.data.ChallengeScheme
import edu.umro.ScalaUtil.PrettyXML

/**
 * Manage and cache the list of results.
 *
 * Results are retrieved from the AQA server on a per-patient basis.
 *
 * Each set is the list of results for that patient.
 */
object Results extends Logging {

  private val prettyPrinter = new PrettyPrinter(1024, 2)

  private def resultsDir = new File(ClientConfig.DataDir, "Results")

  /**
   * List of results that have been processed.
   */
  private val resultList = scala.collection.mutable.HashMap[String, Elem]()

  /**
   * Save the patient results to a disk as a debugging aid.
   */
  private def persist(patientId: String, elem: Elem) = {
    try {
      val file = new File(resultsDir, FileUtil.replaceInvalidFileNameCharacters(patientId, '_') + ".xml")
      val text = PrettyXML.xmlToText(elem)
      FileUtil.writeFile(file, text)
      logger.info("Saved results to " + file.getAbsolutePath)
    } catch {
      case t: Throwable => logger.warn("Unexpected exception saving to Results dir: " + fmtEx(t))
    }
  }

  /**
   * The AQAClient does not have a copy of the outputs for this patient, so it needs to get
   * them from the server.
   *
   * Returns a Left(String) on error.
   */
  private def updatePatient(patientId: String): Either[String, Elem] = {
    val url = ClientConfig.AQAURL + "/GetSeries?PatientID=" + patientId
    val elem = HttpsClient.httpsGet(url, ClientConfig.AQAUser, ClientConfig.AQAPassword, ChallengeScheme.HTTP_BASIC, true) match {
      case Left(exception) => {
        logger.warn("Unable to fetch result list for patient " + patientId + " : " + fmtEx(exception))
        Left("Unable to fetch result list for patient " + patientId + " : " + exception)
      }
      case Right(representation) => {
        val outStream = new ByteArrayOutputStream
        representation.write(outStream)
        val e = XML.loadString(outStream.toString)
        logger.info("Retrieved " + (e \ "Series").size + " results for patient " + patientId)
        //logger.info("\n\nseries list:\n" + (new scala.xml.PrettyPrinter(1024, 2)).format(e) + "\n\n")
        resultList.synchronized { resultList.put(patientId, e) }
        logger.info("patientId: " + patientId + "     number of series: " + (e \ "Series" \ "SeriesInstanceUID").size)
        persist(patientId, e)
        Right(e)
      }
    }
    elem
  }

  private def getPatientResultList(patientId: String): Either[String, Elem] = resultList.synchronized {
    resultList.get(patientId) match {
      case Some(elem) => Right(elem)
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

  /**
   * Given a patient and series UID, get the procedure that was used to process the series.
   *
   * Possible return values:
   *
   *     Right(Some) : success, got the procedure
   *     Right(None) : no processing has been done for that procedure
   *     Left(String) : error, either unable to connect to server or the procedure given by the server could not be identified.
   */
  def getProcedureOfSeries(patientId: String, SeriesInstanceUID: String): Either[String, Option[Procedure]] = {

    def extractProcedure(seriesNode: Node) = {
      ((seriesNode \ "Procedure").headOption) match {
        case Some(proc) => {
          val serverProcName = proc.text
          val procOpt = Procedure.getProcedure(serverProcName)
          if (procOpt.isDefined) Right(procOpt)
          else Left("Could not identify procedure for patient " + patientId + " with series UID " + serverProcName + " that server describes as: " + serverProcName)
        }
        case _ => Right(None)
      }
    }

    getPatientResultList(patientId) match {
      case Left(msg) => Left(msg)
      case Right(elem) => {
        (elem \ "Series" \ "SeriesInstanceUID").find(n => n.head.text.equals(SeriesInstanceUID)) match {
          case Some(seriesNode) => extractProcedure(seriesNode)
          case _ => Right(None)
        }
      }
    }
  }

  /**
   * Return true if the SeriesInstanceUID is in the results.
   */
  def containsSeries(patientId: String, SeriesInstanceUID: String): Either[String, Boolean] = {
    getPatientResultList(patientId) match {
      case Right(list) => {
        val seriesNodeList = list \ "Series" \ "SeriesInstanceUID"
        val doesContain = seriesNodeList.find(n => n.head.text.equals(SeriesInstanceUID)).nonEmpty
        Right(doesContain)
      }
      case Left(msg) => Left(msg)
    }
  }

  /**
   * Return true if there is an RTPLAN with the given FrameOfReferenceUID is in the results.
   */
  def containsPlanWithFrameOfReferenceUID(patientId: String, FrameOfReferenceUID: String): Either[String, Boolean] = {
    getPatientResultList(patientId) match {
      case Right(list) => {
        val doesContain = (list \ "Series").find(n => {
          (n \ "Modality").head.text.equals(ModalityEnum.RTPLAN.toString) &&
            (n \ "FrameOfReferenceUID").head.text.equals(FrameOfReferenceUID)
        }).nonEmpty
        Right(doesContain)
      }
      case Left(msg) => Left(msg)
    }
  }

  /**
   * Initialize by getting series for all patients from the AQA server.
   */
  def init = {
    resultsDir.mkdirs
    logger.info("initializing PatientIDList")
    val count = PatientIDList.getPatientIDList.map(patId => updatePatient(patId)).size
    logger.info("Retrieved series lists for " + count + " patients from the AQA server.")
  }
}