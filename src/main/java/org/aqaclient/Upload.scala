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

  /**
   * A complete set of DICOM series that can be uploaded.
   */
  private case class UploadSet(procedure: ProcedureEnum.Value, imageSeries: Series, reg: Option[Series] = None, plan: Option[Series] = None);

  private def upload(dicomSet: UploadSet): Unit = {
    val prefix = dicomSet.procedure.toString + " : " + dicomSet.imageSeries
    Trace.trace("got upload: " + prefix)

    (dicomSet.reg, dicomSet.plan) match {
      case (Some(reg), Some(plan)) => Trace.trace(prefix + " : " + reg.FrameOfReferenceUID.get + " / " + reg.RegFrameOfReferenceUID.get + "    RTPLAN: " + plan.FrameOfReferenceUID.get)
      case (Some(plan), None) => Trace.trace(prefix + "    RTPLAN: " + plan.FrameOfReferenceUID.get)
      case (None, None) => Trace.trace(prefix)
      case _ => logger.error("Unexpected set of parameters: " + prefix + " : " + dicomSet.reg + " : " + dicomSet.plan)
    }
    Trace.trace
  }

  private def getCT = Series.getByModality(ModalityEnum.CT)

  /**
   * If the given CT's frame of reference matches an RTPLAN, then upload it.
   */
  private def connectWithPlanbyFrameOfRef(ct: Series): Option[UploadSet] = {
    if (ct.isModality(ModalityEnum.CT)) {
      val localPlan = Series.getRtplanByFrameOfReference(ct.FrameOfReferenceUID.get)
      val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, ct.FrameOfReferenceUID.get)

      (localPlan, remotePlan) match {
        case (Some(rtplan), _) => Some(new UploadSet(ProcedureEnum.DailyQACT, ct, Some(rtplan))) // upload CT and RTPLAN
        case (_, true) => Some(new UploadSet(ProcedureEnum.DailyQACT, ct)) // upload just the CT.  The RTPLAN has already been uploaded
        case _ => None // no plan available that has the same frame of reference as this CT
      }
    } else
      None
  }

  /**
   * If there is a CT-REG pair that connect to an RTPLAN, then upload it.  Only upload the RTPLAN as necessary.
   */

  private def connectWithPlanViaReg(ct: Series): Option[UploadSet] = {
    if (ct.isModality(ModalityEnum.CT)) {
      val regOpt = Series.getRegByFrameOfReference(ct.FrameOfReferenceUID.get)
      if (regOpt.isDefined) {
        val reg = regOpt.get
        val localPlan = Series.getRtplanByFrameOfReference(ct.FrameOfReferenceUID.get)
        val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, ct.FrameOfReferenceUID.get)

        (localPlan, remotePlan) match {
          case (Some(rtplan), _) => Some(new UploadSet(ProcedureEnum.DailyQACT, ct, Some(reg), Some(rtplan))) // upload CT, REG, and RTPLAN
          case (_, true) => Some(new UploadSet(ProcedureEnum.DailyQACT, ct, Some(reg))) // upload just the CT and REG.  The RTPLAN has already been uploaded
          case _ => None
        }
      } else
        None
    } else
      None
  }

  /**
   * Get the procedure that this series should be processed with.  First try by looking at the plan it references to see what that
   * was processed with.  If that fails, then just look at the number of slices in the series and make an assumption.
   */
  private def getRtimageProcedure(rtimage: Series): ProcedureEnum.Value = {
    val procByResult: Option[ProcedureEnum.Value] = {
      if (rtimage.ReferencedRtplanUID.isDefined) {
        val proc = Results.getProcedureOfSeries(rtimage.PatientID, rtimage.ReferencedRtplanUID.get)
        // if DailyQACT, then it is DailyQARTIMAGE
        if (proc.isDefined && ProcedureEnum.DailyQACT.toString.equals(proc.get.toString))
          Some(ProcedureEnum.DailyQARTIMAGE)
        else
          proc
      } else
        None
    }

    if (procByResult.isDefined)
      procByResult.get
    else {
      if (rtimage.dir.list.size > 8) ProcedureEnum.Phase2 else ProcedureEnum.DailyQARTIMAGE
    }
  }

  /**
   * Make UploadSet from RTIMAGE series.
   */
  private def uploadableRtimage(rtimage: Series): Option[UploadSet] = {
    if (rtimage.isModality(ModalityEnum.RTIMAGE)) {
      val procedure = getRtimageProcedure(rtimage)
      Some(new UploadSet(procedure, rtimage))
    } else
      None
  }

  /**
   * Given an image series, try to make an upload set.
   */
  private def seriesToUploadSet(series: Series): Option[UploadSet] = {
    val a = connectWithPlanbyFrameOfRef(series)
    if (a.isDefined) a
    else {
      val b = connectWithPlanViaReg(series)
      if (b.isDefined) b
      else uploadableRtimage(series)
    }
  }

  /**
   * Look for test-ready sets of series and upload them.
   */
  private def update = {

    val list = Series.getByModality(ModalityEnum.CT) ++ Series.getByModality(ModalityEnum.RTIMAGE).sortBy(_.dataDate)
    val nextToUpload = list.find(series => seriesToUploadSet(series).isDefined)

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
