package org.aqaclient

import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PrettyXML
import org.restlet.data.ChallengeScheme

import java.io.ByteArrayOutputStream
import scala.xml.Node
import scala.xml.XML

class Procedure(val node: Node) extends Logging {
  // jjjjj
  val Version: String = (node \ "@Version").head.text.trim
  val Name: String = (node \ "@Name").head.text.trim
  /**
   * full URL to run procedure.  The AutoUpload parameter indicates this http client is not a human and that
   * the call should not return until processing is finished.
   */
  val URL: String = ClientConfig.AQAURL + (node \ "@URL").head.text.trim + "?Run=Run&AutoUpload=true&Await=true"

  val toXml: String = PrettyXML.xmlToText(node)

  final def toText: String = Name + " " + Version

  final val isBBbyCBCT = Name.toLowerCase.contains("bb") && Name.toLowerCase.contains("cbct")
  final val isBBbyEPID = Name.toLowerCase.contains("bb") && Name.toLowerCase.contains("epid")
  final val isPhase2 = Name.toLowerCase.matches(".*phase *2.*")
  final val isLOC = (Name.toLowerCase.contains("loc") || Name.toLowerCase.contains("leaf offset")) && (!Name.toLowerCase.contains("base"))
  final val isLOCBaseline = Name.toLowerCase.contains("loc") && Name.toLowerCase.contains("base")

  override def toString: String = {
    Name + ":" + Version
  }

  logger.info("Constructed procedure " + toString +
    "    isBBbyCBCT: " + isBBbyCBCT.toString.head +
    "    isBBbyEPID: " + isBBbyEPID.toString.head +
    "    isPhase2: " + isPhase2.toString.head +
    "    isLOC: " + isLOC.toString.head +
    "    isLOCBaseline: " + isLOCBaseline.toString.head +
    "    URL: " + URL)
}

/**
 * Enumerate different types of procedures. Note that procedures are different
 * from RTPLAN types, because a single type of RTPLAN may be used for different
 * procedures.
 */
object Procedure extends Logging {

  private var procedureList: Seq[Procedure] = null

  def init(): Unit = {
    logger.info("Initializing list of procedures")
    val url = ClientConfig.AQAURL + "/run/WebRunIndex?list=true"

    val elem = HttpsClient.httpsGet(url, ClientConfig.AQAUser, ClientConfig.AQAPassword, ChallengeScheme.HTTP_BASIC, trustKnownCertificates = true, ClientConfig.httpsClientParameters, timeout_ms = ClientConfig.HttpsGetTimeout_ms) match {
      case Left(exception) =>
        logger.warn("Unable to fetch list of procedures to run list for patient: " + fmtEx(exception))
        <RunList></RunList>
      case Right(representation) =>
        val outStream = new ByteArrayOutputStream
        representation.write(outStream)
        val e = XML.loadString(outStream.toString)
        logger.info("Retrieved list of " + (e \ "Run").size + " procedures.")
        logger.info("\n\nRun list:\n" + new scala.xml.PrettyPrinter(1024, 2).format(e) + "\n\n")
        e
    }
    procedureList = (elem \ "Run").map(node => new Procedure(node))
  }

  def getProcedure(ref: String): Option[Procedure] = procedureList.find(p => p.toText.equalsIgnoreCase(ref))

  lazy val BBbyCBCT: Option[Procedure] = procedureList.find(_.isBBbyCBCT)
  lazy val BBbyEPID: Option[Procedure] = procedureList.find(_.isBBbyEPID)
  lazy val Phase2: Option[Procedure] = procedureList.find(_.isPhase2)
  lazy val LOC: Option[Procedure] = procedureList.find(_.isLOC)
  lazy val LOCBaseline: Option[Procedure] = procedureList.find(_.isLOCBaseline)
}
