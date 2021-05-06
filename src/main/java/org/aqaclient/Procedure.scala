package org.aqaclient

import edu.umro.ScalaUtil.Logging

import scala.xml.Node

class Procedure(val node: Node) extends Logging {
  val Version: String = (node \ "Version").head.text.trim
  val Name: String = (node \ "Name").head.text.trim
  val URL: String = (node \ "URL").head.text.trim + "?Run=Run&AutoUpload=true&Await=true"

  final val isBBbyCBCT = Name.toLowerCase.contains("bb") && Name.toLowerCase.contains("cbct")
  final val isBBbyEPID = Name.toLowerCase.contains("bb") && Name.toLowerCase.contains("epid")
  final val isPhase2 = Name.toLowerCase.matches(".*phase *2.*")
  final val isLOC = (Name.toLowerCase.contains("loc") || Name.toLowerCase.contains("leaf offset")) && (!Name.toLowerCase.contains("base"))
  final val isLOCBaseline = Name.toLowerCase.contains("loc") && Name.toLowerCase.contains("base")

  override def toString: String = {
    Name + ":" + Version
  }

  /*
  logger.info(
    "Constructed procedure " + toString +
      "    isBBbyCBCT: " + isBBbyCBCT.toString.head +
      "    isBBbyEPID: " + isBBbyEPID.toString.head +
      "    isPhase2: " + isPhase2.toString.head +
      "    isLOC: " + isLOC.toString.head +
      "    isLOCBaseline: " + isLOCBaseline.toString.head +
      "    URL: " + URL
  )
  */
}
