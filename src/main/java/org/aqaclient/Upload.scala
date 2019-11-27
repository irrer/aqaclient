package org.aqaclient

import edu.umro.ScalaUtil.Logging
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import edu.umro.ScalaUtil.Trace

/**
 * Group series into sets of data that can be processed and upload to the AQA platform.
 */

class Upload extends Actor with Logging {
  override def receive = {
    case notification: Any => {
      logger.info("Upload received notification of new Series.  Total: " + Series.size)
      Upload.update
    }
  }
}

object Upload extends Logging {

  private def upload(procedure: ProcedureEnum.Value, imageSeries: Series, regOrPlan: Option[Series] = None, plan: Option[Series] = None): Unit = {
    val prefix = procedure.toString + " : " + imageSeries
    Trace.trace("got upload: " + prefix)

    (regOrPlan, plan) match {
      case (Some(reg), Some(plan)) => Trace.trace(prefix + " : " + reg.FrameOfReferenceUID.get + " / " + reg.RegFrameOfReferenceUID.get + "    RTPLAN: " + plan.FrameOfReferenceUID.get)
      case (Some(plan), None) => Trace.trace(prefix + "    RTPLAN: " + plan.FrameOfReferenceUID.get)
      case (None, None) => Trace.trace(prefix)
      case _ => logger.error("Unexpected set of parameters: " + prefix + " : " + regOrPlan + " : " + plan)
    }
    Trace.trace
  }

  private def getCT = Series.getByModality(ModalityEnum.CT)

  private def connectWithPlanbyFrameOfRef = {
    /**
     * If the given CT's frame of reference matches an RTPLAN, then upload it.
     */
    def connectWithPlanbyFrameOfRef(ct: Series) = {
      val localPlan = Series.getRtplanByFrameOfReference(ct.FrameOfReferenceUID.get)
      val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, ct.FrameOfReferenceUID.get)

      (localPlan, remotePlan) match {
        case (Some(rtplan), _) => upload(ProcedureEnum.DailyQACT, ct, Some(rtplan)) // upload CT and RTPLAN
        case (_, true) => upload(ProcedureEnum.DailyQACT, ct) // upload just the CT.  The RTPLAN has already been uploaded
        case _ => // no plan available that has the same frame of reference as this CT
      }
    }

    getCT.map(ct => connectWithPlanbyFrameOfRef(ct))
  }

  /**
   * If there is a CT-REG pair that connect to an RTPLAN, then upload it.  Only upload the RTPLAN as necessary.
   */
  private def searchForCtWithReg = {

    def connectWithPlanViaReg(ct: Series) = {
      val regOpt = Series.getRegByFrameOfReference(ct.FrameOfReferenceUID.get)
      if (regOpt.isDefined) {
        val reg = regOpt.get
        val localPlan = Series.getRtplanByFrameOfReference(ct.FrameOfReferenceUID.get)
        val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, ct.FrameOfReferenceUID.get)

        (localPlan, remotePlan) match {
          case (Some(rtplan), _) => upload(ProcedureEnum.DailyQACT, ct, Some(reg), Some(rtplan)) // upload CT, REG, and RTPLAN
          case (_, true) => upload(ProcedureEnum.DailyQACT, ct, Some(reg)) // upload just the CT and REG.  The RTPLAN has already been uploaded
          case _ =>
        }
      }
    }

    val j = getCT // TODO rm
    Trace.trace("getCT: " + j.mkString("\n"))
    getCT.map(ct => {
      connectWithPlanViaReg(ct)
    })
  }

  /**
   * Upload all RTIMAGE series.
   */
  private def searchForRtimage = {
    Series.getByModality(ModalityEnum.RTIMAGE).map(reg => upload(ProcedureEnum.DailyQARTIMAGE, reg))
  }

  /**
   * Look for test-ready sets of series and upload them.
   */
  private def update = {
    connectWithPlanbyFrameOfRef
    searchForCtWithReg
    searchForRtimage
  }

  private val system = ActorSystem("Upload")
  private val uploadActor = system.actorOf(Props[Upload], name = "uploadActor")

  /**
   * Indicate that new data is available for processing.  There may or may not be a set that can be
   * processed.  This function sends a message to the Upload actor and returns immediately.
   */
  def scanSeries = {
    uploadActor ! "update"
  }

  def init = {
    update
  }

}
