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

import edu.umro.EventNetClient.EventNetClient
import edu.umro.EventNetClient.EventNetClientConfig
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PrettyXML

import scala.xml.Elem
import scala.xml.XML

object EventReceiver extends Logging {

  private lazy val eventNetClient: Option[EventNetClient] = {
    try {
      val eventNetConfig = new EventNetClientConfig(ClientConfig.AMQPBrokerHost, ClientConfig.AMQPBrokerPort, "gbtopic", "admintopic", "Aria.Event.")

      val ec = new EventNetClient(eventNetConfig, "AQAClient", 10, 10 * 1000)
      logger.info("EventNet is operational and using broker " + ClientConfig.AMQPBrokerHost + ":" + ClientConfig.AMQPBrokerPort)
      Some(ec)
    } catch {
      case t: Throwable =>
        logger.warn("EventNet is not being enabled.  Unable to connect to broker at " + ClientConfig.AMQPBrokerHost + ":" + ClientConfig.AMQPBrokerPort + " : " + fmtEx(t))
        None
    }
  }

  val queueNameList = Seq("Aria.Event.EventPlanApproval", "Aria.Event.EventPlanStatus")

  def updatePatient(data: Array[Byte]): Unit = {
    val document = XML.loadString(new String(data))
    val PatientId = (document \ "PatientId").head.text
    logger.info("Received event " + document.label + " for PatientId: " + PatientId)
    val patientProcedure = PatientProcedure.getPatientProcedureList.find(pp => pp.patientId.trim.equalsIgnoreCase(PatientId.trim))
    if (patientProcedure.isDefined) {
      logger.info("Retrieving updated list of DICOM series for PatientId: " + PatientId)
      DicomProcessing.updatePatient(patientProcedure.get)
    } else
      logger.info("PatientId " + PatientId + " is not in the patient list. Ignoring event.")
  }

  /** Send an event. */
  def sendEvent(document: Elem): Unit = {
    val routingKey = "Aria.Event." + document.label
    val data = PrettyXML.xmlToText(document).getBytes
    eventNetClient match {
      case Some(ec) => ec.sendEvent("gbtopic", routingKey, data)
      case _        => ;
    }
  }

  /**
    * Initialize by connecting to the AMQP broker and starting listeners.
    */
  def init(): Unit = {

    if (false) { // TODO put back in when the EventNet interface is fixed.
      if (eventNetClient.isDefined) {
        logger.info("Starting EventNet interface.")
        queueNameList.foreach(queueName => eventNetClient.get.consumeNonDurable("gbtopic", queueName, updatePatient))
      } else
        logger.info("EventNet is disabled")
    } else
      logger.info("EventNet is disabled until the Scala interface can be fixed.")
  }

}
