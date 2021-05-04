package org.aqaclient

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Logging
import org.restlet.data.ChallengeScheme

import java.io.ByteArrayOutputStream
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
      (TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime)
    )

    val date =
      try {
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

  /**
    * Get text via HTTPS from the server.
    *
    * @param url Full URL.
    * @return Text on success, nothing on failure.
    */
  def httpsGet(url: String): Option[String] = {
    HttpsClient.httpsGet(
      url,
      ClientConfig.AQAUser,
      ClientConfig.AQAPassword,
      ChallengeScheme.HTTP_BASIC,
      trustKnownCertificates = true,
      ClientConfig.httpsClientParameters,
      timeout_ms = ClientConfig.HttpsGetTimeout_ms
    ) match {
      case Left(exception) =>
        logger.warn("Unable to fetch from url: " + url + " : " + fmtEx(exception))
        None
      case Right(representation) =>
        val outStream = new ByteArrayOutputStream
        representation.write(outStream)
        val text = outStream.toString
        logger.info("Retrieved text from server:\n" + text)
        Some(text)
    }
  }

}
