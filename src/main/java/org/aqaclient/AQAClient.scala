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
import edu.umro.ScalaUtil.PeriodicRestart

import java.util.Date

/**
  * Main entry point for client service.
  */
object AQAClient extends Logging {

  /** Time at which service was started. */
  val serviceStartTime: Long = System.currentTimeMillis

  /**
    * Main entry point for service.
    *
    * @param args Ignored
    */
  def main(args: Array[String]): Unit = {

    try {
      val msg = "AQAClient starting at " + ClientUtil.timeHumanFriendly(new Date(serviceStartTime))
      println(msg)
      logger.info(msg)
      //Util.showJarFile(this)

      if (ClientConfig.validate) {
        logger.info("Validated configuration")
        HttpsInit.init()
        logger.info("Initialized HTTPS")
        DicomMove.init()
        logger.info("Retrieved PatientID list")
        PatientProcedure.init()
        logger.info("Initialized PatientProcedure list")
        Results.init()
        logger.info("Initialized Results repository")
        Series.init()
        logger.info("Initialized Series repository")
        ConfirmDicomComplete.init()
        logger.info("Initialized ConfirmDicomComplete")
        DicomProcessing.init()
        logger.info("Initialized DicomProcessing")
        Upload.init()
        logger.info("Started Upload")
        EventReceiver.init()
        logger.info("Initialized EventReceiver")
        new ClientWebServer
        logger.info("Initialized ClientWebServer")
        MachineLog.init()
        logger.info("Initialized MachineLog")
        new PeriodicRestart(ClientConfig.RestartTime)
        logger.info("Initialized PeriodicRestart")
        logger.info("AQAClient fully initialized")
      }
    } catch {
      // Exceptions thrown to this level should not happen, and if they do it probably means that something
      // unexpected and very serious has happened.
      //
      // If there is a problem, catch and log the error, delay, and then exit with a failed status.  The
      // failed status will tell the service wrapper to restart the service.  The delay is there in the
      // event that this service behaves badly and keeps exiting when started, an keeps the service from
      // using excessive resources.
      case e: Exception =>
        logger.error("Unexpected exception in main: " + fmtEx(e))
        Thread.sleep(30 * 1000)
        System.exit(1)
      case t: Throwable =>
        logger.error("Unexpected throwable in main: " + fmtEx(t))
        Thread.sleep(30 * 1000)
        System.exit(2)
    }
  }
}
