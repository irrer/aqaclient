package org.aqaclient

import java.text.SimpleDateFormat
import java.util.Date
import java.io.File
import scala.xml.Node
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.Logging

object ClientUtil extends Logging {
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
  def dataDateTime(al: AttributeList): Option[Date] = {
    val dateTimeTagPairList = Seq(
      (TagFromName.RTPlanDate, TagFromName.RTPlanTime),
      (TagFromName.ContentDate, TagFromName.ContentTime),
      (TagFromName.AcquisitionDate, TagFromName.AcquisitionTime),
      (TagFromName.SeriesDate, TagFromName.SeriesTime),
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime))

    def getDateTime(dateTag: AttributeTag, timeTag: AttributeTag): Option[Date] = {
      try {
        val d = al.get(dateTag).getSingleStringValueOrNull
        val t = al.get(timeTag).getSingleStringValueOrNull
        val ms = DicomUtil.dicomDateFormat.parse(d).getTime + DicomUtil.parseDicomTime(t).get
        Some(new Date(ms))
      } catch {
        case t: Throwable => None
      }
    }

    val date = try {
      dateTimeTagPairList.map(dtp => getDateTime(dtp._1, dtp._2)).flatten.headOption
    } catch {
      case t: Throwable => throw new RuntimeException("Could not get date+time from DICOM: " + fmtEx(t))
    }
    date
  }

  /**
   * Safely get a list of files in a directory.  On failure, return an empty list.
   */
  def listFiles(dir: File): List[File] = {
    try {
      dir.listFiles.toList
    }
    catch {
      case t: Throwable => List[File]()
    }
  }
}