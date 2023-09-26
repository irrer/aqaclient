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
import edu.umro.RestletUtil.TrustingSslContextFactory
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.Trace
import org.apache.http.ConnectionClosedException
import org.restlet.data.ChallengeScheme
import org.restlet.representation.Representation
import org.restlet.resource.ClientResource

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
      (TagByName.CreationDate, TagByName.CreationTime)
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

  @tailrec
  private def makeUniqueZipFile(description: String): File = {
    val file = {
      val name = timeAsFileNameFormat.format(new Date) + "_" + description + ".zip"
      val fileName = FileUtil.replaceInvalidFileNameCharacters(name replace (' ', '_'), '_')
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

  // ---------------------------------------------------------------------------------------------------
  // ---------------------------------------------------------------------------------------------------
  // ---------------------------------------------------------------------------------------------------
  // ---------------------------------------------------------------------------------------------------

  import org.restlet.data.ChallengeResponse
  import org.restlet.data.Cookie

  /**
    * Establish the client resource, setting up the certificate trust model as the caller specified.
    *
    * @param url                    : URL of service
    * @param trustKnownCertificates : If true, select the list of certificates specified with the <code>TrustKnownCertificates.init</code>
    *                               function. Defaults to false.
    */
  private def getClientResource(url: String, trustKnownCertificates: Boolean, parameterList: Map[String, String], cookieList: Seq[Cookie]) = {
    val clientResource = if (trustKnownCertificates) {
      val clientContext = new org.restlet.Context
      clientContext.getAttributes.put("sslContextFactory", new TrustingSslContextFactory)
      new ClientResource(clientContext, url)
    } else new ClientResource(url)

    if (parameterList.nonEmpty) {
      val parameters = clientResource.getContext.getParameters
      parameterList.keys.map(key => parameters.add(key, parameterList(key)))
    }

    if (cookieList.nonEmpty) {
      val requestCookieList = clientResource.getRequest.getCookies
      requestCookieList.clear()
      cookieList.map(c => requestCookieList.add(c))
    }
    clientResource
  }

  //noinspection ConvertNullInitializerToUnderscore
  private var cr: ClientResource = null

  private def httpsGet(
      url: String,
      userId: String = "",
      password: String = "",
      challengeScheme: ChallengeScheme = ChallengeScheme.HTTP_BASIC,
      trustKnownCertificates: Boolean = false,
      parameterList: Map[String, String] = Map(),
      timeout_ms: Option[Long] = None,
      cookieList: Seq[Cookie] = Seq()
  ): Either[Throwable, Representation] = {

    if (cr == null)
      cr = getClientResource(url, trustKnownCertificates, parameterList, cookieList = cookieList)
    val challengeResponse = new ChallengeResponse(challengeScheme, userId, password)
    cr.setChallengeResponse(challengeResponse)
    val result = Right(cr.get) // perform(clientResource.get _, timeout_ms)
    Trace.trace(s"result: $result")
    result
  }

  def main(args: Array[String]): Unit = {
    println("Starting")
    val urlList: Seq[String] = Seq(
      "https://upload.wikimedia.org/wikipedia/commons/a/a5/Flower_poster_2.jpg",
      "https://www.pcclean.io/wp-content/gallery/roma-wallpapers/Roma-22.jpg",
      "https://upload.wikimedia.org/wikipedia/commons/8/8b/Ft5_3mb.JPG",
      "https://upload.wikimedia.org/wikipedia/commons/f/fd/Municipal_Market_of_S%C3%A3o_Paulo_city.jpg",
      "https://uhroappwebspr1.umhs.med.umich.edu:8111/GetSeries?PatientID=$TB1_OBI_2023Q1",
      "https://uhroappwebspr1.umhs.med.umich.edu:8111/GetSeries?PatientID=$TB5_OBI_2021Q3",
      "https://automatedqualityassurance.org/GetSeries?PatientID=$AQA_TB1"
    )

    if (true) {
      // val helloClientResource = new ClientResource("https://www.google.com/")
      val helloClientResource = new ClientResource(urlList.head)

      for (i <- Seq(0, 1, 2, 3)) {
        val bao = new ByteArrayOutputStream()
        val url = urlList(i)
        helloClientResource.setReference(url)
        helloClientResource.get.write(bao)
        println(s"$url    -->   length: ${bao.toByteArray.length} ")
      }
      System.exit(99)
    }

    if (false) {
      // val helloClientResource = new ClientResource("https://www.google.com/")
      val helloClientResource = new ClientResource(urlList(2))
      helloClientResource.setReference("hey")

      // helloClientResource.get.write(System.out)
      for (i <- 1 until 100) {
        val baos = new ByteArrayOutputStream()
        helloClientResource.get.write(baos)
        println(s"i: ${i.formatted("%3d")}    length: ${baos.toByteArray.length}")
      }
      System.exit(99)
    }

    ClientConfig.validate

    def hGet(url: String): Option[String] = {
      val result = httpsGet(
        url,
        ClientConfig.AQAUser,
        ClientConfig.AQAPassword,
        ChallengeScheme.HTTP_BASIC,
        trustKnownCertificates = true,
        ClientConfig.httpsClientParameters,
        timeout_ms = ClientConfig.HttpsGetTimeout_ms
      )

      if (result.isLeft)
        None
      else {
        val representation = result.right.get
        Thread.sleep(10)
        Trace.trace(s"representation size: ${representation.getSize}")

        // val stringListener = new StringReadingListener(representation) { override def onContent(content: String): Unit = println(s"Size of new content: ${content.length}") }

        // representation.setListener(stringListener)
        // representation.exhaust()
        Trace.trace(s"representation.getAvailableSize: ${representation.getAvailableSize}")

        Thread.sleep(1000)
        val inBytes: Array[Byte] = Array.ofDim[Byte](3240935 + 1024)

        if (true) { // this gives 78% success rate
          val inputStream = representation.getStream
          @tailrec
          def getBytes(total: Int = 0): Int = {
            val size = inputStream.read(inBytes)
            size match {
              case _ if size > 0 => getBytes(total + size)
              case 0 =>
                Trace.trace("did it sleep waiting for data")
                Thread.sleep(100)
                getBytes(total + size)
              case -1 => total
            }
          }

          try {
            val inSize = getBytes()
            Trace.trace(s"inputStream size: $inSize")
            Some("hey")
          } catch {
            case cc: ConnectionClosedException =>
              Trace.trace("ConnectionClosedException: " + fmtEx(cc))
              None
            case t: Throwable =>
              Trace.trace("ConnectionClosedException: " + fmtEx(t))
              None

          }

        } else { // this give 3% success rate

          val reader = representation.getReader
          val buf = Array.ofDim[Char](3240935 + 1024)

          @tailrec
          def getBytes(total: Int = 0): Int = {
            val size = reader.read(buf)
            size match {
              case _ if size > 0 => getBytes(total + size)
              case 0 =>
                Thread.sleep(100)
                getBytes(total + size)
              case -1 => total
            }
          }

          try {
            val size = getBytes()
            Trace.trace(s"reader size: $size")
            Some("hey")
          } catch {
            case cc: ConnectionClosedException =>
              Trace.trace("ConnectionClosedException: " + fmtEx(cc))
              None
            case t: Throwable =>
              Trace.trace("ConnectionClosedException: " + fmtEx(t))
              None

          }
        }
        // val text = result.right.get.getText
      }
    }

    for (i <- 0 until 100) {
      Thread.sleep(500)
      // val url = urlList(i % urlList.size)
      val url = urlList.head
      val text = hGet(url)
      // val text = httpsGet("http://localhost/GetSeries?PatientID=$TB5_OBI_2021Q3")

      if (text.isEmpty) {
        println(s"$i : did it failed!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!    $url")
        Thread.sleep(500)
      } else {
        if (text.get == null) {
          println(s"$i : did it What!!!!!!!!!!!!!!!!!!!!!!!!!!    $url")
        } else {
          val length = text.get.length
          if (length == 0)
            println(s"$i : did it length is 0 !!!!!!!!!!!!!!!!!!!!!!!!!!    $url")
          else
            println(
              s"$i : did it success ============================== text.length" +
                s": $length    $url"
            )
        }
      }
    }
    /*
     */

    println("Sleeping...")
    Thread.sleep(10 * 60 * 60 * 1000.toLong)
    println("Done")
  }
}
