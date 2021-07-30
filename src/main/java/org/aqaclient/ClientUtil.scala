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
import com.pixelmed.dicom.TagFromName
import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.Trace
import org.restlet.data.ChallengeScheme
import org.restlet.data.MediaType
import org.restlet.representation.Representation

import java.io.ByteArrayOutputStream
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Node

object ClientUtil extends Logging {
  private val timeHumanFriendlyFormat = new SimpleDateFormat("EEE MMM dd yyyy HH:mm:ss Z")

  def timeHumanFriendly(date: Date): String = timeHumanFriendlyFormat.format(date)

  val defaultDateTime = new Date(24 * 60 * 60 * 1000)

  val timeAsFileNameFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss-SSS")

  /**
    * Get the jar file that contains this class.
    */
  lazy val thisJarFile: File = {
    val theClass = this.getClass // Pick current jar.  For a different jar pick a class from that jar.
    new File(theClass.getProtectionDomain.getCodeSource.getLocation.toURI)
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

  // for synchronizing HTTP calls
  private val sync = "sync"

  /**
    * Get text via HTTPS from the server.
    *
    * @param url Full URL.
    * @return Text on success, nothing on failure.
    */
  def httpsGet(url: String): Option[String] = {
    try {
      val start = System.currentTimeMillis()
      val result = sync.synchronized {
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
            logger.warn("Unable to fetch from: " + url + " : " + fmtEx(exception))
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
            logger.info("Successfully completed HTTPS get of " + text.length + " bytes from " + url + " in " + elapsed + " ms.  text: " + text.take(200))
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

    val result = ClientUtil.sync.synchronized {
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
    result
  }
}
