package org.aqaclient

import edu.umro.ScalaUtil.Logging

import java.util.concurrent.TimeoutException
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Failure

object DicomSemaphore extends Logging {

  /** Used to limit use of DICOM C-MOVEs and C-FINDs to one. */
  private val dicomAqaClientSynchronize = "dicomAqaClientSynchronize "

  /**
    * Shut down the service after a delay.   The delay is to avoid problems where the
    * system shuts down only to start up and fail again.
    * @param description Description of DICOM request.
    */
  private def delayThenRestart(description: String): Unit = {
    val restartDelay_sec = 30.0
    val restartDelay_ms = (restartDelay_sec * 1000).toLong
    logger.error(s"Restarting service in $restartDelay_sec seconds due to DICOM timeout for $description.")
    Thread.sleep(restartDelay_ms)
    logger.error(s"Shutting down service to restart due to DICOM timeout for $description.")
    System.exit(1)
  }

  /**
    * Run the given DICOM function within a Future that enforces timeout.  If the
    * function times out, then restart this service.
    * @param func Call this function to do some sort of DICOM thing.
    * @param description Description of what the function is doing (for logging purposes).
    * @return
    */
  def processInSemaphore[T](func: () => Seq[T], description: String): Seq[T] = {

    dicomAqaClientSynchronize.synchronized {
      val start = System.currentTimeMillis()
      try {
        Thread.sleep(50) // do not overload the server
        val dicomFuture: Future[Seq[T]] = Future { func() }

        Await.ready(dicomFuture, ClientConfig.DicomTimeout_ms.toInt.millisecond)

        val dicomResult = dicomFuture.value

        val elapsed_ms = System.currentTimeMillis() - start

        // In case of failure, also print the cause of the exception, when defined
        dicomResult match {
          case Some(Failure(exception)) =>
            logger.error(s"DICOM operation $description failed after elapsed time of $elapsed_ms ms with exception: ${fmtEx(exception)}")
            Seq()
          case data =>
            if (data.isDefined && data.get.isSuccess) {
              logger.info(s"DICOM operation $description succeeded after elapsed time of $elapsed_ms ms ")
              val list = data.get.get
              list
            } else {
              logger.error(s"DICOM operation $description failed after elapsed time of $elapsed_ms ms ")
              Seq()
            }
        }
      } catch {
        // If the dicomResult value did not complete within 1 second, the call
        // to `Await.ready` throws a TimeoutException
        case _: TimeoutException =>
          val elapsed_ms = System.currentTimeMillis() - start
          println(s"DICOM operation $description timed out after $elapsed_ms ms.")
          delayThenRestart(description)
          Seq()
      }
    }
  }

}
