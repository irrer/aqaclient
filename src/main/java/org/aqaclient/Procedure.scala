package org.aqaclient

import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.Logging
import java.io.ByteArrayOutputStream
import scala.xml.XML
import scala.xml.Elem
import scala.xml.Node

class Procedure(node: Node) {
  val Version = (node \ "@Version").head.text
  val Name = (node \ "@Name").head.text
  val URL = ClientConfig.AQAURL + "/" + (node \ "@URL").head.text + "?Run=Run&AutoUpload=true" // full URL to run procedure.  The AutoUpload parameter indicates this http client is not a human.

  final def toText = Name + " " + Version

  final val isBBbyCBCT = Name.toLowerCase.contains("bb") && Name.toLowerCase.contains("cbct")
  final val isBBbyEPID = Name.toLowerCase.contains("bb") && Name.toLowerCase.contains("epid")
  final val isPhase2 = Name.toLowerCase.contains("phase2")
  final val isLOC = (Name.toLowerCase.contains("loc") || Name.toLowerCase.contains("leaf offset")) && (!Name.toLowerCase.contains("base"))
  final val isLOCBaseline = URL.toLowerCase.contains("loc") && Name.toLowerCase.contains("base")
}

/**
 * Enumerate different types of procedures. Note that procedures are different
 * from RTPLAN types, because a single type of RTPLAN may be used for different
 * procedures.
 */
object Procedure extends Logging {

  private var procedureList: Seq[Procedure] = null

  def init =
    {
      logger.info("Initializing list of procedures")
      val url = ClientConfig.AQAURL + "/run/WebRunIndex?list=true"

      val elem = HttpsClient.httpsGet(url, ClientConfig.AQAUser, ClientConfig.AQAPassword) match {
        case Left(exception) => {
          logger.warn("Unable to fetch list of procedures to run list for patient: " + fmtEx(exception))
          <RunList></RunList>
        }
        case Right(representation) => {
          val outStream = new ByteArrayOutputStream
          representation.write(outStream)
          val e = XML.loadString(outStream.toString)
          logger.info("Retrieved list of " + (e \ "Run").size + " procedures.")
          logger.info("\n\nRun list:\n" + (new scala.xml.PrettyPrinter(1024, 2)).format(e) + "\n\n")
          e
        }
      }
      procedureList = (elem \ "Run").map(node => new Procedure(node))
    }

  def getProcedure(ref: String) = procedureList.find(p => p.toText.equalsIgnoreCase(ref))

  lazy val BBbyCBCT = procedureList.filter(_.isBBbyCBCT).head
  lazy val BBbyEPID = procedureList.filter(_.isBBbyEPID).head
  lazy val Phase2 = procedureList.filter(_.isPhase2).head
  lazy val LOC = procedureList.filter(_.isLOC).head
  lazy val LOCBaseline = procedureList.filter(_.isLOCBaseline).head
}
