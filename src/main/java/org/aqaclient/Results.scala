package org.aqaclient

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PrettyXML

import java.io.File
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Elem
import scala.xml.XML

/**
  * Manage and cache the list of results.
  *
  * Results are retrieved from the AQA server on a per-patient basis.
  *
  * Each set is the list of results for that patient.
  */
object Results extends Logging {

  private def resultsDir = new File(ClientConfig.DataDir, "Results")

  /**
    * List of results that have been processed.
    */
  private val resultList = scala.collection.mutable.HashMap[String, Elem]()

  /**
    * Save the patient results to a disk as a debugging aid.
    */
  private def persist(patientId: String, elem: Elem): Unit = {
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
    */
  @tailrec
  private def updatePatient(patientId: String): Elem = {
    val url = ClientConfig.AQAURL + "/GetSeries?PatientID=" + patientId
    logger.info("Getting list of series for PatientID " + patientId)

    ClientUtil.httpsGet(url) match {
      case Some(text) =>
        logger.info("Retrieved list of series for PatientID " + patientId)
        try {
          val e = XML.loadString(text)
          resultList.synchronized { resultList.put(patientId, e) }
          logger.info("patientId: " + patientId + "     number of series: " + (e \ "Series" \ "SeriesInstanceUID").size)
          persist(patientId, e)
          e
        } catch {
          case t: Throwable =>
            logger.warn("Unexpected error getting list of outputs.  Will retry in " + ClientConfig.HttpsGetTimeout_sec + " seconds. PatientID:  " + patientId + " : " + fmtEx(t))
            Thread.sleep(ClientConfig.HttpsGetTimeout_ms.get)
            updatePatient(patientId)
        }
      case _ =>
        logger.warn("Unable to fetch list of series for PatientID " + patientId + " .  Will retry in " + ClientConfig.HttpsGetTimeout_sec + " seconds.")
        Thread.sleep(ClientConfig.HttpsGetTimeout_ms.get)
        updatePatient(patientId)
    }
  }

  private def getPatientResultList(patientId: String): Elem =
    resultList.synchronized {
      resultList.get(patientId) match {
        case Some(elem) => elem
        case _          => updatePatient(patientId)
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
    * Return true if the SeriesInstanceUID is in the results.
    */
  def containsSeries(patientId: String, SeriesInstanceUID: String): Boolean = {
    val seriesNodeList = getPatientResultList(patientId) \ "Series" \ "SeriesInstanceUID"
    val doesContain = seriesNodeList.exists(n => n.head.text.trim.equals(SeriesInstanceUID))
    doesContain
  }

  def containsSeries(series: Series): Boolean = containsSeries(series.PatientID, series.SeriesInstanceUID)

  /**
    * Return true if there is an RTPLAN with the given FrameOfReferenceUID is in the results.
    */
  def containsPlanWithFrameOfReferenceUID(patientId: String, FrameOfReferenceUID: String): Boolean = {
    val doesContain = (getPatientResultList(patientId) \ "Series").exists(n => {
      (n \ "Modality").head.text.trim.equals(ModalityEnum.RTPLAN.toString) &&
        (n \ "FrameOfReferenceUID").head.text.trim.equals(FrameOfReferenceUID)
    })
    doesContain
  }

  /**
    * Initialize by getting series for all patients from the AQA server.
    */
  def init(): Unit = {
    resultsDir.mkdirs
    logger.info("initializing PatientIDList")
    PatientProcedure.patientIdList.foreach(updatePatient)
    logger.info("Number of patients: " + PatientProcedure.patientIdList.size)
  }
}
