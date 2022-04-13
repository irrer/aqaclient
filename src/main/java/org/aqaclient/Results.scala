/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqaclient

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PrettyXML
import edu.umro.ScalaUtil.Trace

import java.io.File
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
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

  /** Retry getting results from the server until this time before giving up. */
  private def timeoutGet = (ClientConfig.HttpsGetTimeout_ms.get * 5) + System.currentTimeMillis()

  /**
    * List of results that have been processed.
    */
  private val resultList = scala.collection.mutable.HashMap[String, Elem]()

  private def resultsFile(patientId: String) = new File(resultsDir, FileUtil.replaceInvalidFileNameCharacters(patientId, '_') + ".xml")

  /**
    * Save the patient results to a disk as a debugging aid.
    */
  private def persist(patientId: String, elem: Elem): Unit = {
    try {
      val file = resultsFile(patientId)
      val text = PrettyXML.xmlToText(elem)
      FileUtil.writeFile(file, text)
      logger.info("Saved results to " + file.getAbsolutePath)
    } catch {
      case t: Throwable => logger.warn("Unexpected exception saving to Results dir: " + fmtEx(t))
    }
  }

  /**
    * Get the results from the server.  On failure return None.
    * @param patientId Get for this patient.
    *
    * @return Result text or None.
    */
  private def getHttpTextFromServer(patientId: String): Option[String] = {
    val url = ClientConfig.AQAURL + "/GetSeries?PatientID=" + patientId
    logger.info("Getting list of series for PatientID " + patientId)

    // get results from server.  Even if there are no results, it should return an empty list, so
    // 'None' means HTTP failure.  Also protect against exception from HTTP GET.
    val httpText: Option[String] = {
      try {
        val t = ClientUtil.httpsGet(url)
        t
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected error getting list of outputs.  Will retry in " + ClientConfig.HttpsGetTimeout_sec + " seconds. PatientID:  " + patientId + " : " + fmtEx(t))
          None
      }
    }
    httpText
  }

  /**
    * The AQAClient does not have a copy of the outputs for this patient, so it needs to get
    * them from the server.
    */
  @tailrec
  private def updatePatient(patientId: String, timeout: Long): Elem = {

    val httpText = getHttpTextFromServer(patientId)
    if (System.currentTimeMillis() < timeout) {

      httpText match {
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
              updatePatient(patientId, timeout)
          }
        case _ =>
          logger.warn("Unable to fetch list of series for PatientID " + patientId + " .  Will retry in " + ClientConfig.HttpsGetTimeout_sec + " seconds.")
          Thread.sleep(ClientConfig.HttpsGetTimeout_ms.get)
          updatePatient(patientId, timeout)
      }
    } else {
      logger.warn("Timed out getting results from server.  Giving up and using latest results")
      // if there are previous results, use them, else return an empty list.
      val fileText = FileUtil.readTextFile(resultsFile(patientId))
      if (fileText.isRight) {
        val e = XML.loadString(fileText.right.get)
        resultList.synchronized { resultList.put(patientId, e) }
        e
      } else {
        logger.info("Unable to get previous results list for patient " + patientId + " .   Caching an empty list of results for that patient.")
        val e = emptyList
        resultList.synchronized { resultList.put(patientId, e) }
        e
      }
    }
  }

  // a patient result list with no items
  private val emptyList = { <SeriesList></SeriesList> }

  private def getPatientResultList(patientId: String): Elem =
    resultList.synchronized {
      resultList.get(patientId) match {
        case Some(elem) => elem
        case _          => emptyList
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
  def refreshPatient(patientId: String): Unit = {
    Future {
      updatePatient(patientId, timeoutGet)
    }
  }

  /**
    * Return true if the SeriesInstanceUID is in the cached results.
    */
  def containsSeries(patientId: String, SeriesInstanceUID: String): Boolean = {
    def isInList(list: NodeSeq) = {
      (list \ "Series").exists(n => (n \ "SeriesInstanceUID").text.trim.equals(SeriesInstanceUID))
    }
    val doesContain = resultList.synchronized {
      val list = resultList.get(patientId)
      list.isDefined && isInList(list.get)
    }
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
    * Get the modality of a node.
    * @param n For this node.
    * @return Name of modality.
    */
  private def modalityOf(n: Node): String = (n \ "Modality").head.text.trim

  /**
    * Get a list all series nodes
    */
  private def seriesSeq = resultList.synchronized { resultList.values.toList }.flatMap(p => (p \ "Series").flatten)

  /**
    * Given a node, return the procedure it referendes.
    * @param n Node from results.
    * @return Procedure or None.
    */
  private def procedureOfNode(n: Node): Option[Procedure] = {
    Trace.trace(PrettyXML.xmlToText(n)) // TODO rm
    (n \ "Procedure").map(_.text.trim).flatMap(Procedure.procedureByName).headOption
  }

  /**
    * Find the RTPLAN that has the given SOPInstanceUID.
    * @param rtplanInstanceUid SOPInstanceUID of one of the members of the RTPLAN series.
    * @return Matching rtplan node, or None.
    */
  private def findRtplanBySopInstanceUid(rtplanInstanceUid: String): Option[Node] = {
    def rtplanWithUid(n: Node): Boolean = {
      val jj = PrettyXML.xmlToText(n) // TODO rm
      if (modalityOf(n).equals("RTPLAN")) {
        val sopList = (n \ "SOPInstanceUIDList" \ "SOPInstanceUID").map(_.text.trim)
        sopList.contains(rtplanInstanceUid)
      } else
        false
    }

    seriesSeq.find(rtplanWithUid)
  }

  /**
    * Given an RTPLAN UID, return the procedure it was used for in previous results.
    *
    * @param rtplanUID SOP Instance UID of RTPLAN.
    *
    * @return Procedure, if it can be found.
    */
  def getProcedureByRtplan(rtplanUID: String): Option[Procedure] = {

    def byRtplan = {
      findRtplanBySopInstanceUid(rtplanUID) match {
        case Some(rtplanNode) =>
          Trace.trace(PrettyXML.xmlToText(rtplanNode)) // TODO rm
          procedureOfNode(rtplanNode)
        case _ => None
      }
    }

    /**
      * Find the procedure by looking at other series that reference this plan and
      * return their procedure.
      * @return Procedure of another series that references the given RTPLAN.
      */
    def byOtherImageSeries: Option[Procedure] = {

      def referencesRtplan(n: Node): Boolean = {
        val ref = (n \ "referencedRtplanUID").map(_.text.trim)
        ref.contains(rtplanUID)
      }

      seriesSeq.find(referencesRtplan) match {
        case Some(n) =>
          Trace.trace(PrettyXML.xmlToText(n)) // TODO rm
          procedureOfNode(n)
        case _ => None
      }
    }

    // First try looking at the rtplan directly to see if it is known and if it has a procedure defined.  If so, use it.
    //
    // If not, look for another series that references the RTPLAN and if found and if it has a procedure defined, use it.
    //
    // There is also the (unlikely) possibility that a procedure will not be found.
    val procedure = byRtplan match {
      case Some(procedure) =>
        Some(procedure)
      case _ => byOtherImageSeries
    }
    procedure
  }

  /**
    * Forcibly get the latest results for each patient.
    */
  def refreshAll(): Unit = {
    PatientProcedure.patientIdList.foreach(p => refreshPatient(p))
  }

  /**
    * Periodically update the results in case anything has happened on the server (manual execution of
    * test or deletion of results) that the client would not be aware of.
    */
  private class PeriodicUpdate extends Runnable {
    def run(): Unit = {
      while (true) {
        logger.info("Updating all patient results.")
        refreshAll()
        Thread.sleep(ClientConfig.ResultsRefreshInterval_ms)
      }
    }
  }

  /**
    * Initialize by getting series for all patients from the AQA server.
    */
  def init(): Unit = {
    resultsDir.mkdirs
    logger.info("initializing PatientIDList")
    PatientProcedure.patientIdList.foreach(p => updatePatient(p, timeoutGet))
    logger.info("Number of patients: " + PatientProcedure.patientIdList.size)
    new Thread(new PeriodicUpdate).start()
  }

  def main(args: Array[String]): Unit = {
    init()
    Trace.trace(getProcedureByRtplan("1.2.246.352.71.5.427549902257.853607.20211012135203"))
    Trace.trace(getProcedureByRtplan("1.3.6.1.4.1.22361.17483843774714.1857926758.1641396595237.652"))
    System.exit(0)
  }
}
