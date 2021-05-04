package org.aqaclient

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging

import java.io.File
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.xml.Node
import scala.xml.XML

class PatientProcedure(node: Node) extends Logging {
  val patientId: String = (node \ "PatientID").head.text.trim
  val procedureList: immutable.Seq[Procedure] = (node \ "Procedure").map(e => new Procedure(e))
}

/**
  * CRUD interface for the list of patient IDs used for fetching DICOM files.
  *
  * Currently the only read access.
  */

object PatientProcedure extends Logging {

  def url: String = ClientConfig.AQAURL + "/admin/PatientProcedureXml"

  // Last time (in ms) that list was updated from server.
  private var lastUpdateTime_ms: Long = 0

  private val patientProcedureList = ArrayBuffer[PatientProcedure]()

  /**
    * Write the content to a text file if it has changed.  This is for diagnosing problems and debugging.
    *
    * @param text Latest version of list.
    */
  private def persistList(text: String): Unit = {
    val file = new File(ClientConfig.getDataDir, "PatientProcedureList.xml")
    val currentContent = {
      FileUtil.readTextFile(file) match {
        case Right(oldText) => oldText
        case _              => "NA"
      }
    }

    if (!text.equals(currentContent)) FileUtil.writeFile(file, text)
  }

  /**
    * Get the latest content.
    */
  private def refreshList(): Unit = {
    lastUpdateTime_ms = System.currentTimeMillis()
    ClientUtil.httpsGet(url) match {
      case Some(text) =>
        persistList(text)
        val node = XML.loadString(text)
        val list = (node \ "PatientProcedure").map(n => new PatientProcedure(n))
        patientProcedureList.synchronized {
          patientProcedureList.clear()
          patientProcedureList.appendAll(list)
        }
      case _ => logger.warn("Could not get PatientProcedure content from " + url)
    }
  }

  /**
    * Get a read-only list of patient IDs.  If they are 'old', then get the latest copy form the server.
    */
  private def getPatientProcedureList: List[PatientProcedure] = {
    val age_ms = System.currentTimeMillis() - lastUpdateTime_ms // age of data in ms
    if (age_ms > ClientConfig.PatientProcedureAgeLimit_ms) refreshList()
    patientProcedureList.toList
  }

  def patientIdList: Seq[String] = getPatientProcedureList.map(_.patientId)

  /**
    * Given a patient and series UID, get the procedure that was used to process the series.
    *
    * @return The procedure or nothing.  The latter could be the result of not being able to connect with the server.
    */
  def getProcedureOfSeries(series: Series): Option[Procedure] = {
    // def getProcedureOfSeries(patientId: String, SeriesInstanceUID: String): Option[Procedure] = {

    // get patient procedure related values
    val patientProcedure = PatientProcedure.getPatientProcedureList.find(_.patientId.equals(series.PatientID))
    val ctProcedure = if (patientProcedure.isEmpty) None else patientProcedure.get.procedureList.find(p => p.isBBbyCBCT)
    val rtimageProcedure = if (patientProcedure.isEmpty) None else patientProcedure.get.procedureList.find(p => !p.isBBbyCBCT)

    val procedure =
      0 match {
        case _ if series.isModality(ModalityEnum.CT) && ctProcedure.isDefined           => ctProcedure
        case _ if series.isModality(ModalityEnum.RTIMAGE) && rtimageProcedure.isDefined => rtimageProcedure
        case _                                                                          => None
      }
    procedure
  }

  def init(): Unit = {
    class UpdateLoop extends Runnable {
      override def run(): Unit = {
        refreshList()
        Thread.sleep(ClientConfig.PatientProcedureAgeLimit_ms)
      }
    }
    new Thread(new UpdateLoop).start()
  }

}
