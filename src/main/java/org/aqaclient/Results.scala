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
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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
   * If the server can not be contacted, then wait this long (in seconds) before retrying.
   */
  private val retryInterval_sec = 30

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
  @tailrec
  private def updatePatient(patientId: String): Elem = {
    val url = ClientConfig.AQAURL + "/GetSeries?PatientID=" + patientId
    HttpsClient.httpsGet(url, ClientConfig.AQAUser, ClientConfig.AQAPassword, ChallengeScheme.HTTP_BASIC, true) match {
      case Left(exception) => {
        logger.warn("Unable to fetch result list from server via HTTPS for patient " + patientId + " .  Will retry in " + retryInterval_sec + " seconds.  Exception: " + fmtEx(exception))
        Thread.sleep(retryInterval_sec * 1000)
        updatePatient(patientId)
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
        e
      }
    }
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
   * 
   * After marking the results as stale, this function starts another update for that patient in the
   * background so that it will be ready when needed.
   */
  def markAsStale(patientId: String): Unit = {
    resultList.synchronized {
      resultList -= patientId
    }
    Future {
      getPatientResultList(patientId)
    }
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
  def getProcedureOfSeries(patientId: String, SeriesInstanceUID: String): Option[Procedure] = {

    def extractProcedure(seriesNode: Node): Option[Procedure] = {
      ((seriesNode \ "Procedure").headOption) match {
        case Some(proc) => {
          val serverProcName = proc.text
          val procOpt = Procedure.getProcedure(serverProcName)
          if (procOpt.isDefined) procOpt
          else {
            logger.error("Could not identify procedure for patient " + patientId +
              " with series UID " +
              serverProcName +
              " that server describes as: " +
              serverProcName +
              " Series node: " + prettyPrinter.format(seriesNode))
            None
          }
        }
        case _ => None
      }
    }

    (getPatientResultList(patientId) \ "Series" \ "SeriesInstanceUID").find(n => n.head.text.equals(SeriesInstanceUID)) match {
      case Some(seriesNode) => extractProcedure(seriesNode)
      case _ => None

    }
  }

  /**
   * Return true if the SeriesInstanceUID is in the results.
   */
  def containsSeries(patientId: String, SeriesInstanceUID: String): Boolean = {
    val seriesNodeList = getPatientResultList(patientId) \ "Series" \ "SeriesInstanceUID"
    val doesContain = seriesNodeList.find(n => n.head.text.equals(SeriesInstanceUID)).nonEmpty
    doesContain
  }

  /**
   * Return true if there is an RTPLAN with the given FrameOfReferenceUID is in the results.
   */
  def containsPlanWithFrameOfReferenceUID(patientId: String, FrameOfReferenceUID: String): Boolean = {
    val doesContain = (getPatientResultList(patientId) \ "Series").find(n => {
      (n \ "Modality").head.text.equals(ModalityEnum.RTPLAN.toString) &&
        (n \ "FrameOfReferenceUID").head.text.equals(FrameOfReferenceUID)
    }).nonEmpty
    doesContain
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