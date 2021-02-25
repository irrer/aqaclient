package org.aqaclient

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Logging

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Node

object ClientUtil extends Logging {
  private val timeHumanFriendlyFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss Z")

  def timeHumanFriendly(date: Date): String = timeHumanFriendlyFormat.format(date)

  val defaultDateTime = new Date(24 * 60 * 60 * 1000)

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
  def getAttr(node: Node, name: String): String = (node \ ("@" + name)).text

  /**
   * Get SeriesInstanceUID
   */
  def getSerUid(al: AttributeList): Option[String] = {
    val serUid = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrEmptyString
    if (serUid.isEmpty) None else Some(serUid)
  }

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

  /**
   * Make a best effort to get the date (date+time) from the given attribute list.
   *
   * On failure return None.
   */
  def dataDateTime(al: AttributeList): Date = {
    val dateTimeTagPairList = Seq(
      (TagFromName.RTPlanDate, TagFromName.RTPlanTime),
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      (TagFromName.SeriesDate, TagFromName.SeriesTime),
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime))

    val date = try {
      dateTimeTagPairList.flatMap(dtp => DicomUtil.getTimeAndDate(al, dtp._1, dtp._2)).headOption
    } catch {
      case t: Throwable => throw new RuntimeException("Could not get date+time from DICOM: " + fmtEx(t))
    }
    if (date.isDefined) date.get else defaultDateTime
  }

  /**
   * Safely get a list of files in a directory.  On failure, return an empty list.
   */
  def listFiles(dir: File): List[File] = {
    try {
      dir.listFiles.toList
    } catch {
      case _: Throwable => List[File]()
    }
  }
}