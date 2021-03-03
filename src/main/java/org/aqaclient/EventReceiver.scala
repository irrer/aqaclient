package org.aqaclient

import edu.umro.EventNetClient.EventNetClient
import edu.umro.EventNetClient.EventNetClientConfig
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PrettyXML
import edu.umro.ScalaUtil.Trace

import scala.xml.Elem
import scala.xml.XML

object EventReceiver extends Logging {
  Trace.trace("starting")

  private lazy val eventNetClient: Option[EventNetClient] = {
    try {
      Trace.trace()
      val eventNetConfig = new EventNetClientConfig(ClientConfig.AMQPBrokerHost, ClientConfig.AMQPBrokerPort, "gbtopic", "admintopic", "Aria.Event.")
      Trace.trace()

      val ec = new EventNetClient(eventNetConfig, "AQAClient", 10, 10 * 1000)
      logger.info("EventNet is operational and using broker " + ClientConfig.AMQPBrokerHost + ":" + ClientConfig.AMQPBrokerPort)
      Trace.trace()
      Some(ec)
    } catch {
      case t: Throwable =>
        logger.warn("EventNet is not being enabled.  Unable to connect to broker at " + ClientConfig.AMQPBrokerHost + ":" + ClientConfig.AMQPBrokerPort + " : " + fmtEx(t))
        Trace.trace()
        None
    }
  }

  val queueNameList = Seq("Aria.Event.EventPlanApproval", "Aria.Event.EventPlanStatus")

  def updatePatient(data: Array[Byte]): Unit = {
    Trace.trace()
    val document = XML.loadString(new String(data))
    Trace.trace()
    val PatientId = (document \ "PatientId").head.text
    Trace.trace()
    logger.info("Received event " + document.label + " for PatientId: " + PatientId)
    if (PatientIDList.getPatientIDList.exists(p => p.trim.equalsIgnoreCase(PatientId.trim))) {
      Trace.trace()
      logger.info("Retrieving updated list of DICOM series for PatientId: " + PatientId)
      DicomProcessing.updatePatient(PatientId)
    } else
      logger.info("PatientId " + PatientId + " is not listed in the " + PatientIDList.PatientIDFile.getAbsolutePath + " file. Ignoring event.")
    Trace.trace()
  }

  /** Send an event. */
  def sendEvent(document: Elem): Unit = {
    Trace.trace
    val routingKey = "Aria.Event." + document.label
    Trace.trace()
    val data = PrettyXML.xmlToText(document).getBytes
    Trace.trace()
    eventNetClient match {
      case Some(ec) => ec.sendEvent("gbtopic", routingKey, data)
      case _        => ;
    }
    Trace.trace()
  }

  /**
    * Initialize by connecting to the AMQP broker and starting listeners.
    */
  def init(): Unit = {
    if (eventNetClient.isDefined) {
      Trace.trace()
      logger.info("Starting EventNet interface.")
      Trace.trace()
      queueNameList.foreach(queueName => eventNetClient.get.consumeNonDurable("gbtopic", queueName, updatePatient))
      Trace.trace()
    } else
      logger.info("EventNet is disabled")
    Trace.trace()
  }

}
