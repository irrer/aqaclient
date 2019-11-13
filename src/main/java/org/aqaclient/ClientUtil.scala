package org.aqaclient

import java.text.SimpleDateFormat
import java.util.Date
import java.io.File
import scala.xml.Node
import com.pixelmed.dicom.AttributeList

object ClientUtil {
  private val timeHumanFriendlyFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss Z")

  def timeHumanFriendly(date: Date) = timeHumanFriendlyFormat.format(date)

  /**
   * Get the jar file that contains this class.
   */
  lazy val thisJarFile: File = {
    val clss = this.getClass // Pick current jar.  For a different jar pick a class from that jar.
    new File(clss.getProtectionDomain.getCodeSource.getLocation.toURI)
  }

  /**
   * Get an attribute of a node as text.
   */
  def getAttr(node: Node, name: String) = (node \ ("@" + name)).text.toString

  /**
   * Read a DICOM file.
   */
  def readDicomFile(file: File): Either[Throwable, AttributeList] = {
    try {
      val al = new AttributeList
      al.read(file)
      Right(al)
    } catch {
      case t: Throwable => Left(t)
    }
  }

}