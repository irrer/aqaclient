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

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.Trace
import org.restlet.data.ChallengeScheme
import org.restlet.data.MediaType
import org.restlet.representation.Representation

import java.io.ByteArrayOutputStream
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.annotation.tailrec

object ClientUtil extends Logging {
  private val timeHumanFriendlyFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss Z")

  def timeHumanFriendly(date: Date): String = timeHumanFriendlyFormat.format(date)

  private val defaultDateTime = new Date(24 * 60 * 60 * 1000)

  val timeAsFileNameFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss-SSS")

  /**
    * Get the jar file that contains this class.
    */
  lazy val thisJarFile: File = {
    val theClass = this.getClass // Pick current jar.  For a different jar pick a class from that jar.
    new File(theClass.getProtectionDomain.getCodeSource.getLocation.toURI)
  }

  /**
    * Get SeriesInstanceUID
    */
  def getSerUid(al: AttributeList): Option[String] = {
    val serUid = al.get(TagByName.SeriesInstanceUID).getSingleStringValueOrEmptyString
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
      (TagByName.RTPlanDate, TagByName.RTPlanTime),
      (TagByName.ContentDate, TagByName.ContentTime),
      (TagByName.AcquisitionDate, TagByName.AcquisitionTime),
      (TagByName.SeriesDate, TagByName.SeriesTime),
      (TagByName.InstanceCreationDate, TagByName.InstanceCreationTime)
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

  // for synchronizing HTTP calls
  private val syncAQAClientHTTP = "syncAQAClientHTTP"

  /**
    * Get text via HTTPS from the server.
    *
    * @param url Full URL.
    * @return Text on success, nothing on failure.
    */
  def httpsGet(url: String): Option[String] = {
    try {
      logger.info("Performing GET from " + url)
      val start = System.currentTimeMillis()
      val result = syncAQAClientHTTP.synchronized {
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
            logger.warn("Unable to GET from: " + url + " : " + fmtEx(exception))
            None
          case Right(representation) =>
            val outStream = new ByteArrayOutputStream
            representation.write(outStream)
            val text = outStream.toString
            representation.exhaust()

            val stream = representation.getStream
            Trace.trace("representation.getStream: " + stream)
            if (stream != null) {
              Trace.trace("Closing representation stream")
              stream.close()
              Trace.trace("Closed representation stream")
            }
            val elapsed = System.currentTimeMillis() - start
            logger.info("Successfully completed HTTPS GET of " + text.length + " bytes from " + url + " in " + elapsed + " ms.  text: " + text.take(200))
            Some(text)
        }
      }
      result
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error while performing HTTPS GET from " + url + " : " + fmtEx(t))
        None
    }
  }

  /**
    * Upload the files.  Do this in a synchronized so that no other HTTP
    * activity from this service is being attempted while it waits.
    *
    * @param url Where to send the data.
    * @param zipFile Data to upload
    * @return Either an error (left) or success (right).
    */
  def httpsPost(url: String, zipFile: File): Either[Throwable, Representation] = {

    logger.info("Performing POST to " + url + " with zip file of size " + zipFile.length())
    val start = System.currentTimeMillis()
    val result = ClientUtil.syncAQAClientHTTP.synchronized {
      HttpsClient.httpsPostSingleFileAsMulipartForm(
        url,
        zipFile,
        MediaType.APPLICATION_ZIP,
        ClientConfig.AQAUser,
        ClientConfig.AQAPassword,
        ChallengeScheme.HTTP_BASIC,
        trustKnownCertificates = true,
        ClientConfig.httpsClientParameters,
        timeout_ms = ClientConfig.HttpsUploadTimeout_ms
      )
    }
    val elapsed = System.currentTimeMillis() - start
    logger.info("Performed POST to " + url + " with zip file of size " + zipFile.length() + "    elapsed ms: " + elapsed)
    result
  }

  @tailrec
  private def makeUniqueZipFile(description: String): File = {
    val file = {
      val name = timeAsFileNameFormat.format(new Date) + "_" + description + ".zip"
      val fileName = FileUtil.replaceInvalidFileNameCharacters(name replace(' ', '_'), '_')
      new File(ClientConfig.zipDir, fileName)
    }
    if (file.isFile) {
      Thread.sleep(1000)
      makeUniqueZipFile(description)
    } else
      file
  }

  /**
    * Make a temporary zipped file out of a list of files.
    * @param fileList Files to zip.
    * @return Zipped file containing the fileList.
    */
  def makeZipFile(fileList: Seq[File], description: String): File = {

    logger.info("fileList.size: " + fileList.size + " : " + fileList.map(_.getAbsolutePath).mkString("    "))

    val out = new ByteArrayOutputStream
    FileUtil.readFileTreeToZipStream(fileList.sortBy(_.getName), Seq[String](), Seq[File](), out)

    val zipFile = makeUniqueZipFile("size_" + out.size() + "_" + description)

    val bytes = out.toByteArray
    FileUtil.writeBinaryFile(zipFile, bytes)
    logger.info("wrote zip file " + zipFile.getName + "    size: " + bytes.length)
    zipFile
  }

}
