package org.aqaclient

import java.util.Date
import edu.umro.ScalaUtil.PeriodicRestart
import edu.umro.ScalaUtil.Logging
import org.aqaclient.series.Series

/**
 * Main entry point for client service.
 */
object AQAClient extends Logging {
  /** Time at which service was started. */
  val serviceStartTime = System.currentTimeMillis

  def main(args: Array[String]): Unit = {

    try {
      val msg = "AQAClient starting at " + ClientUtil.timeHumanFriendly(new Date(serviceStartTime))
      println(msg)
      logger.info(msg)
      //Util.showJarFile(this)

      if (ClientConfig.validate) {
        DicomMove.init
        PatientIDList.init
        Series.init
        DicomProcessing.init
        WebUpload.init
        new ClientWebServer
        new PeriodicRestart(ClientConfig.RestartTime)
        logger.info("AQAClient started")
      }
    } catch {
      // Exceptions thrown to this level should not happen, and if they do it probably means that something
      // unexpected and very serious has happened.
      //
      // If there is a problem, catch and log the error, delay, and then exit with a failed status.  The
      // failed status will tell the service wrapper to restart the service.  The delay is there in the
      // event that this service behaves badly and keeps exiting when started, an keeps the service from
      // using excessive resources.
      case e: Exception => {
        logger.error("Unexpected exception in main: " + fmtEx(e))
        Thread.sleep(30 * 1000)
        System.exit(1)
      }
      case t: Throwable => {
        logger.error("Unexpected throwable in main: " + fmtEx(t))
        Thread.sleep(30 * 1000)
        System.exit(2)
      }
    }
  }
}
