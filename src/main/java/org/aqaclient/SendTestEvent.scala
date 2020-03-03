package org.aqaclient

import edu.umro.EventNetClient.EventNetClient
import edu.umro.EventNetClient.EventNetClientConfig
import edu.umro.ScalaUtil.Trace
import scala.xml.Elem
import edu.umro.ScalaUtil.PrettyXML
import scala.xml.XML
import edu.umro.ScalaUtil.Logging

object SendTestEvent {

  val testEvent = {
    <EventPlanApproval xmlns="urn:EventPlanApproval">
      <PatientId>MobiusDailyQA</PatientId>
      <DoctorId/>
      <CourseId>C1</CourseId>
      <CourseSer>253285</CourseSer>
      <PlanSetupId>MobiusDailyQA</PlanSetupId>
      <PlanSetupSer>671007</PlanSetupSer>
      <PlanUid>1.2.246.352.71.5.427549902257.671007.20190904102856</PlanUid>
      <Status>PlanApproval</Status>
      <ApprovalUserId>umhs\rkashani</ApprovalUserId>
      <Header>
        <AgentId>d9b35427-6708-40e3-956b-1dab94442b6b</AgentId>
        <AgentName>AllEvent.Listen</AgentName>
        <EventId>99f9b5fc-8ea3-4c1f-b6ed-9f7904b77f79</EventId>
        <EventDateTime>2019-09-04T10:30:15</EventDateTime>
        <EventSourceName>UHROARIASPR1:10.30.65.100</EventSourceName>
        <InResponseTo>661b7fc1-0e32-4746-83e0-3c9a61b51a2a</InResponseTo>
        <IpAddress>10.30.3.92</IpAddress>
        <ProcessName>edu.umro.aria.winservice.evtlisten.messaging</ProcessName>
        <KeyName>PlanSetupSer</KeyName>
        <KeyValue>671007</KeyValue>
      </Header>
    </EventPlanApproval>
  }

  def main(args: Array[String]): Unit = {
    Trace.trace("starting")
    Trace.trace("Config validated: " + ClientConfig.validated)
    Trace.trace

    object FmtT extends Logging {
      def ft(t: Throwable) = fmtEx(t)
    }

    Trace.trace
    val eventNetConfig = new EventNetClientConfig(ClientConfig.AMQPBrokerHost, ClientConfig.AMQPBrokerPort, "gbtopic", "admintopic", "Aria.Event.")
    Trace.trace
    val eventNetClient = new EventNetClient(eventNetConfig, "SendTestEvent", 10, 10 * 1000)
    Trace.trace

    /** Send an event. */
    def sendEvent(document: Elem): Unit = {
      Trace.trace
      val routingKey = "Aria.Event." + document.label
      Trace.trace
      val data = PrettyXML.xmlToText(document).getBytes
      Trace.trace
      eventNetClient.sendEvent("gbtopic", routingKey, data)
      Trace.trace
    }

    Trace.trace

    Seq(1, 2).map(i => {
      Trace.trace
      Thread.sleep(1000)
      Trace.trace
      sendEvent(testEvent)
      Trace.trace
    })

    Thread.sleep(100)
    Trace.trace("Sleeping ...")
    Thread.sleep(2 * 1000)
    Trace.trace("Exiting")
    System.exit(99)

  }
}