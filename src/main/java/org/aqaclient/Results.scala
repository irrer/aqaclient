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
  def getHttpTextFromServer(patientId: String): Option[String] = {
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

  /**
    * Put value in list.  For testing only.
    * @param patientId Patient ID.
    * @param e Results for patient.
    */
  def testPut(patientId: String, e: Elem): Unit = {
    resultList.synchronized { resultList.put(patientId, e) }
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
  /*
  private def containsPlanWithFrameOfReferenceUID(patientId: String, FrameOfReferenceUID: String): Boolean = {
    val doesContain = (getPatientResultList(patientId) \ "Series").exists(n => {
      (n \ "Modality").head.text.trim.equals(ModalityEnum.RTPLAN.toString) &&
        (n \ "FrameOfReferenceUID").head.text.trim.equals(FrameOfReferenceUID)
    })
    doesContain
  }
   */

  /**
    * If there is an RTPLAN with the given FrameOfReferenceUID is in the results, then return its procedure.
    */
  def procedureOfPlanWithFrameOfReferenceUID(patientId: String, FrameOfReferenceUID: String): Option[Procedure] = {

    def isPwF(s: Node): Boolean = {

      def isRtplan: Boolean = (s \ "Modality").head.text.trim.equals(ModalityEnum.RTPLAN.toString)

      def sameFrameOfRef: Boolean = {
        val node = s \ "FrameOfReferenceUID"
        node.nonEmpty && node.head.text.trim.equals(FrameOfReferenceUID)
      }

      def hasProcedure: Boolean = (s \ "Procedure").nonEmpty

      try {
        isRtplan && sameFrameOfRef && hasProcedure
      } catch {
        case _: Throwable => false
      }
    }

    val nodeOf: Option[Node] = (getPatientResultList(patientId) \ "Series").find(isPwF)

    if (nodeOf.isEmpty)
      None
    else {
      val procName = (nodeOf.get \ "Procedure").text
      Procedure.procedureByName(procName)
    }
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
    * Given a node, return the procedure it references.
    * @param n Node from results.
    * @return Procedure or None.
    */
  def procedureOfNode(n: Node): Option[Procedure] = {
    (n \ "Procedure").map(_.text.trim).flatMap(Procedure.procedureByName).headOption
  }

  /**
    * Find the RTPLAN that has the given SOPInstanceUID.
    * @param rtplanInstanceUid SOPInstanceUID of one of the members of the RTPLAN series.
    * @return Matching rtplan node, or None.
    */
  def findRtplanBySopInstanceUid(rtplanInstanceUid: String): Option[Node] = {
    def rtplanWithUid(n: Node): Boolean = {
      if (modalityOf(n).equals("RTPLAN")) {
        val sopList = (n \ "SOPInstanceUIDList" \ "SOPInstanceUID").map(_.text.trim)
        val ok = sopList.contains(rtplanInstanceUid)
        ok
      } else
        false
    }

    seriesSeq.find(rtplanWithUid)
  }

  /**
    * Find the RTPLAN that has the given SOPInstanceUID.
    * @param rtplanInstanceUid SOPInstanceUID of one of the members of the RTPLAN series.
    * @return Matching rtplan node, or None.
    */
  def findRtimageByRtplanReference(rtplanInstanceUid: String): Option[Node] = {
    def rtimageReferencingRtplan(n: Node): Boolean = {
      if (modalityOf(n).equals("RTIMAGE")) {
        val rtplanSop = (n \ "referencedRtplanUID").text
        rtplanSop.equals(rtplanInstanceUid)
      } else
        false
    }

    seriesSeq.find(rtimageReferencingRtplan)
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

}
