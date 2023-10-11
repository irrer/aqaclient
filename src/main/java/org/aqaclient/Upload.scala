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

import edu.umro.ScalaUtil.Logging

import scala.annotation.tailrec

/**
  * Group series into sets of data that can be processed and upload to the AQA platform.
  */

object Upload extends Logging {

  /**
    * Send the zip file to the AQA server. Return true on success.  There may later be a failure on
    * the server side, but this success just indicates that the upload was successful.
    */
  private def uploadToAQA(uploadSet: UploadSet): Option[String] = {
    try {
      val start = System.currentTimeMillis
      logger.info("Starting upload of data set to AQA for procedure " + uploadSet.procedure + " " + uploadSet.description)

      // upload the files and wait for the processing to finish.  Do this in a synchronized so that no
      // other HTTP activity from this service is being attempted while it waits.
      val result = HttpUtil.httpsPost(uploadSet.procedure.URL, uploadSet.zipFile)

      val elapsed = System.currentTimeMillis - start
      result match {
        case Right(_) =>
          logger.info("Elapsed time in ms of upload: " + elapsed + "  Successful upload of data set to AQA for " + uploadSet)
          None
        case Left(failure) =>
          logger.warn("Elapsed time in ms of upload: " + elapsed + "  Failed to upload data set to AQA for " + uploadSet + " : " + fmtEx(failure))
          Some("Elapsed time in ms of upload: " + elapsed + "  failed to upload data set to AQA: " + failure.getMessage)
      }
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error while using HTTPS client to upload zip file to AQA: " + fmtEx(t))
        Some("Error communicating with AQA: " + fmtEx(t))
    }
  }

  /**
    * Upload a set of DICOM files for processing.
    */
  @tailrec
  private def upload(uploadSet: UploadSet, retryCount: Int = ClientConfig.MaxUploadRetryCount): Boolean = {
    logger.info("Processing upload " + uploadSet)
    val success: Boolean =
      try {
        logger.info("Beginning upload of " + uploadSet)
        val start = System.currentTimeMillis()
        val msg = uploadToAQA(uploadSet)
        val elapsed = System.currentTimeMillis() - start
        logger.info("Elapsed upload time: " + edu.umro.ScalaUtil.Util.intervalTimeUserFriendly(elapsed) + " : " + uploadSet)
        val ok = msg.isEmpty
        if (ok)
          logger.info("Successfully uploaded " + uploadSet)
        else {
          logger.warn("Failure while uploading " + uploadSet + " : " + msg.get)
        }

        logger.info("Executing post-processing...")
        uploadSet.postProcess(msg)
        logger.info("Finished executing post-processing.")
        // try { uploadSet.zipFile.delete() }
        // catch { case _: Throwable => }
        Thread.sleep((ClientConfig.GracePeriod_sec * 1000).toLong)
        // Series.removeObsoleteZipFiles() // clean up any zip files
        ok
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected exception during upload: " + fmtEx(t))
          false
      }

    if ((!success) && (retryCount > 0)) {
      Thread.sleep(5 * 1000)
      logger.warn("Retrying upload.  Retry count: " + retryCount + "    uploadSet: " + uploadSet)
      upload(uploadSet, retryCount - 1)
    } else success
  }

  private val queue = new java.util.concurrent.LinkedBlockingQueue[UploadSet]

  private def startUpdateThread(): Unit = {
    class Updater extends Runnable {
      def run(): Unit = {
        while (true) {
          val uploadSet = queue.take
          logger.info("Processing new upload " + uploadSet)
          upload(uploadSet)
        }
      }
    }

    new Thread(new Updater).start()
  }

  /**
    * Put an upload set on the FIFO queue to be sent to the server.
    *
    * @param uploadSet Data to upload.
    */
  def put(uploadSet: UploadSet): Unit = {
    queue.put(uploadSet)
    logger.info("Upload " + uploadSet + " queued.  Size of queue: " + queue.size())
  }

  def init(): Unit = {
    logger.info("initializing Upload")
    startUpdateThread()
    logger.info("Upload has been initialized")
  }

}
