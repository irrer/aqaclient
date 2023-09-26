package org.aqaclient

import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.Logging
import org.restlet.data.ChallengeScheme
import org.restlet.data.MediaType
import org.restlet.representation.Representation
import org.restlet.resource.ClientResource

import java.io.ByteArrayOutputStream
import java.io.File

object HttpUtil extends Logging {

  private class ClientRes {
    val clientResource: ClientResource = {
      // @formatter:off
      val challengeResponse = HttpsClient.makeChallengeResponse(
        scheme   = ChallengeScheme.HTTP_BASIC,
        userId   = ClientConfig.AQAUser,
        password = ClientConfig.AQAPassword)

      HttpsClient.makeClientResource(
        ClientConfig.AQAURL,
        Some(challengeResponse),
        trustKnownCertificates = true)
      // @formatter:on
    }
    private val maxAge: Long = 5 * 60 * 1000
    private val created = System.currentTimeMillis()

    private def age = System.currentTimeMillis() - created

    def isOld: Boolean = age > maxAge
  }

  /** Thread-safe place to keep client resource.
   * There should only ever be 0 or 1 entries.;
   * The key is not used, only the value.
   */
  private val clientRes = scala.collection.concurrent.TrieMap[Int, ClientRes]()

  /**
   * Get a valid client resource.
   *
   * @param clear If true, discard the current resource and make a new one
   * @return
   */
  private def getClientRes(clear: Boolean = false): ClientResource = {
    if (clear || clientRes.isEmpty || clientRes.values.head.isOld) {
      clientRes.clear()
      clientRes.put(0, new ClientRes)
    }
    clientRes.values.head.clientResource
  }


  /**
   * Get text via HTTPS from the server.
   *
   * @param url Full URL.
   * @return Text on success, nothing on failure.
   */
  def httpsGet(url: String): Option[String] = {
    try {
      val start = System.currentTimeMillis()
      val result = clientRes.synchronized {
        logger.info("Performing GET from " + url)
        HttpsClient.httpsGet(
          getClientRes(),
          url,
          timeout_ms = ClientConfig.HttpsGetTimeout_ms
        ) match {
          case Left(exception) =>
            getClientRes(clear = true) // don't use this resource again
            val elapsed = System.currentTimeMillis() - start
            logger.warn(s"Unable to GET from: $url    Elapsed ms: $elapsed    ${fmtEx(exception)}")
            None
          case Right(representation) =>
            val outStream = new ByteArrayOutputStream
            representation.write(outStream)
            val text = outStream.toString
            representation.exhaust()

            val stream = representation.getStream
            if (stream != null) {
              stream.close()
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
   * Note on ClientResource: The POST operation is done infrequently and can last a
   * long time, so using a ClientResource from a pool can create problems.  Instead,
   * a new ClientResource is created for each use of POST.
   *
   * @param url     Where to send the data.
   * @param zipFile Data to upload
   * @return Either an error (left) or success (right).
   */
  def httpsPost(url: String, zipFile: File): Either[Throwable, Representation] = {

    val result = clientRes.synchronized {
      logger.info("Performing POST to " + url + " with zip file of size " + zipFile.length())
      val start = System.currentTimeMillis()
      val res = try {
        HttpsClient.httpsPostSingleFileAsMulipartForm(
          (new ClientRes).clientResource, // get a ClientResource for use by this upload only.
          url,
          zipFile,
          MediaType.APPLICATION_ZIP,
          timeout_ms = ClientConfig.HttpsUploadTimeout_ms
        )
      }
      catch {
        case t: Throwable => Left(t)
      }

      val elapsed = System.currentTimeMillis() - start
      logger.info(s"Performed POST to $url with zip file of size ${zipFile.length()}     elapsed ms:    $elapsed    success: ${res.isRight}")
      res
    }

    result
  }

}
