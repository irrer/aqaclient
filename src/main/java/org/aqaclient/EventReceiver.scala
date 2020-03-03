package org.aqaclient

import edu.umro.EventNetClient.EventNetClient
import edu.umro.EventNetClient.EventNetClientConfig
import edu.umro.ScalaUtil.Trace
import scala.xml.Elem
import edu.umro.ScalaUtil.PrettyXML
import scala.xml.XML
import edu.umro.ScalaUtil.Logging

object EventReceiver extends Logging {
  Trace.trace("starting")

  private lazy val eventNetClient: Option[EventNetClient] = {
    try {
      val eventNetConfig = new EventNetClientConfig(ClientConfig.AMQPBrokerHost, ClientConfig.AMQPBrokerPort, "gbtopic", "admintopic", "Aria.Event.")

      val ec = new EventNetClient(eventNetConfig, "AQAClient", 10, 10 * 1000)
      logger.info("EventNet is operational and using broker " + ClientConfig.AMQPBrokerHost + ":" + ClientConfig.AMQPBrokerPort)
      Some(ec)

    } catch {
      case t: Throwable => {
        logger.warn("EventNet is not being enabled.  Unable to connect to broker at " + ClientConfig.AMQPBrokerHost + ":" + ClientConfig.AMQPBrokerPort + " : " + fmtEx(t))
        None
      }
    }
  }

  val queueNameList = Seq(
    "Aria.Event.EventPlanApproval",
    "Aria.Event.EventPlanStatus")

  def updatePatient(data: Array[Byte]): Unit = {
    val document = XML.loadString(new String(data))
    val PatientId = (document \ "PatientId").head.text
    logger.info("Received event " + document.label + " for PatientId: " + PatientId)
    if (PatientIDList.getPatientIDList.find(p => p.trim.equalsIgnoreCase(PatientId.trim)).isDefined) {
      logger.info("Retrieving updated list of DICOM series for PatientId: " + PatientId)
      DicomProcessing.updatePatient(PatientId)
    }
  }

  /** Send an event. */
  def sendEvent(document: Elem): Unit = {
    Trace.trace
    val routingKey = "Aria.Event." + document.label
    val data = PrettyXML.xmlToText(document).getBytes
    eventNetClient match {
      case Some(ec) => ec.sendEvent("gbtopic", routingKey, data)
      case _ => ;
    }
  }

  /**
   * Initialize by connecting to the AMQP broker and starting listeners.
   */
  def init = {
    if (eventNetClient.isDefined) {
      logger.info("Starting EventNet interface.")
      queueNameList.map(queueName => eventNetClient.get.consumeNonDurable("gbtopic", queueName, updatePatient))
    } else
      logger.info("EventNet is disabled")
  }

}