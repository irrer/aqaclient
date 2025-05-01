package org.aqaclient

import edu.umro.ScalaUtil.Logging

import java.util.concurrent.TimeoutException
import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/*
 * Copyright 2024 Regents of the University of Michigan
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

object DicomSemaphore extends Logging {

  /** Used to limit use of DICOM C-MOVEs and C-FINDs to one. */
  private val dicomAqaClientSynchronize = "dicomAqaClientSynchronize "

  /**
    * Shut down the service after a delay.   The delay is to avoid problems where the
    * system shuts down only to start up and fail again.
    * @param description Description of DICOM request.
    */
  private def delayThenResetSocket(close: () => _, description: String): Unit = {
    val restartDelay_sec = 10.0 // an arbitrary delay to let the server quiesce and not hammer it too hard.
    val restartDelay_ms = (restartDelay_sec * 1000).toLong
    logger.error(s"Resetting DICOM connection in $restartDelay_sec seconds due to DICOM failure for $description.")
    Thread.sleep(restartDelay_ms)
    logger.info(s"Closing socket for $description")
    try {
      close()
    } catch {
      case t: Throwable =>
        logger.error(s"Unexpected exception while closing socket for DICOM operation $description : ${fmtEx(t)}")
        if (ClientConfig.ShutdownOnExceptionDuringDicomSocketClose) {
          logger.error(s"Restarting service due in 10 seconds due to Unexpected exception while closing socket for DICOM operation $description ")
          Thread.sleep(10 * 1000)
          logger.error(s"Restarting service NOW due to Unexpected exception while closing socket for DICOM operation $description ")
          System.exit(1)
        }

    }
    logger.info(s"Closed socket for $description")
  }

  /**
    * Run the given DICOM function within a Future that enforces timeout.  If the
    * function times out, then restart this service.
    * @param dicomOp Call this function to do some sort of DICOM thing.
    * @param close Call this function when it is necessary to abort the dicomOp function.  Normally this should not be necessary.
    * @param description Description of what the function is doing (for logging purposes).
    * @return
    */
  def processInSemaphore[T](dicomOp: () => Seq[T], close: () => _, description: String): Seq[T] = {
    def wrappedDicomOp(): Either[Throwable, Seq[T]] = {
      try {
        Right(dicomOp())
      } catch {
        case t: Throwable =>
          logger.error(s"Exception in Future. DICOM $description : ${fmtEx(t)}")
          Left(t)
      }
    }

    def perform(): Option[Seq[T]] = {
      val start = System.currentTimeMillis()
      try {
        Thread.sleep(50) // do not overload the PACS/Varian server
        logger.info(s"DICOM operation starting: $description")

        val dicomFuture = Future { wrappedDicomOp() }
        Await.ready(dicomFuture, ClientConfig.DicomTimeout_ms.toInt.millisecond)
        val dicomResult = dicomFuture.value.get.get

        val elapsed_ms = System.currentTimeMillis() - start

        // In case of failure, also print the cause of the exception, when defined
        dicomResult match {
          case Left(throwable) =>
            logger.error(s"DICOM operation $description failed after elapsed time of $elapsed_ms ms with exception: ${fmtEx(throwable)}")
            logger.error(s"DICOM operation $description aborted.")
            delayThenResetSocket(close, s"DICOM operation threw throwable: $throwable for $description")
            None
          case Right(list) =>
            Some(list)
        }
      } catch {
        // If the dicomResult value did not complete within ClientConfig.DicomTimeout_ms, the call
        // to `Await.ready` throws a TimeoutException
        case _: TimeoutException =>
          val elapsed_ms = System.currentTimeMillis() - start
          logger.error(s"DICOM operation $description timed out after $elapsed_ms ms.")
          delayThenResetSocket(close, s"DICOM operation timed out for $description")
          None

        case t: Throwable =>
          val elapsed_ms = System.currentTimeMillis() - start
          logger.error(s"Unexpected exception for DICOM operation $description after $elapsed_ms ms. : ${fmtEx(t)}")
          delayThenResetSocket(close, s"DICOM operation threw exception: $t for $description")
          None
      }
    }

    /** Number of times to retry the DICOM operation.  */
    val retryCount = 3

    /**
      * Provide retry logic.
      * @param count Number of times left to try.
      * @return
      */
    @tailrec
    def retry(count: Int): Seq[T] = {
      if (count > 0) {
        val result = perform()
        if (result.isDefined)
          result.get
        else {
          logger.error(s"DICOM operation $description failed.  Try: ${(retryCount + 1) - count}.   Connection being reset.")
          close()
          retry(count - 1)
        }
      } else {
        logger.error(s"DICOM operation $description failed after $retryCount tries.  Giving up.")
        Seq()
      }
    }

    dicomAqaClientSynchronize.synchronized {
      logger.info(s"Starting synchronized DICOM operation $description")
      val result = retry(retryCount)
      logger.info(s"Done with synchronized DICOM operation $description    result size: ${result.size}")
      result
    }
  }

}
