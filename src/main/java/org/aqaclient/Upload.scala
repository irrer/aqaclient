package org.aqaclient

import edu.umro.ScalaUtil.Logging
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import edu.umro.ScalaUtil.Trace
import java.io.File
import java.util.Date
import edu.umro.ScalaUtil.FileUtil
import java.io.ByteArrayOutputStream
import org.restlet.ext.html.FormDataSet
import org.restlet.representation.FileRepresentation
import org.restlet.data.MediaType
import org.restlet.ext.html.FormData
import org.restlet.resource.ClientResource
import org.restlet.data.ChallengeResponse
import org.restlet.data.ChallengeScheme

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
   * A complete set of DICOM series that can be uploaded.  A set of series may consist of any of these:
   *
   *     - A CT series                 (with or without an RTPLAN)
   *     - A CT series and REG series  (with or without an RTPLAN)
   *     - An RTIMAGE series           (with or without an RTPLAN)
   */
  case class UploadSet(procedure: Procedure, imageSeries: Series, reg: Option[Series] = None, plan: Option[Series] = None) {
    override def toString = {
      val r = {
        if (procedure.isBBbyCBCT) {
          if (reg.isDefined) "    with reg" else "    no reg"
        } else ""
      }
      val p = if (plan.isDefined) "with plan" else "no plan"
      procedure.toString + "    PatientID: " + imageSeries.PatientID + r + "    " + p
    }

    /**
     * Get a list of all files in this upload set.
     */
    def getAllDicomFiles = {
      def filesOf(series: Option[Series]) = if (series.isDefined) series.get.dir.listFiles.toSeq else Seq[File]()
      val imageFiles = imageSeries.dir.listFiles.toSeq
      imageFiles ++ filesOf(reg) ++ filesOf(plan)
    }
  }

  private def makeZipFile(fileList: Seq[File]): File = {
    val baos = new ByteArrayOutputStream
    FileUtil.readFileTreeToZipStream(fileList, Seq[String](), Seq[File](), baos)
    val zipFileName = FileUtil.replaceInvalidFileNameCharacters(edu.umro.ScalaUtil.Util.dateToText(new Date) + ".zip", '_')
    val zipFile = new File(ClientConfig.zipDir, zipFileName)
    FileUtil.writeBinaryFile(zipFile, baos.toByteArray)
    zipFile
  }

  /**
   * Send the zip file to the AQA server. Return true on success.  There may later be a failure on
   * the server side, but this success just indicates that the upload was successful.
   */
  private def uploadToAQA(procedure: Procedure, zipFile: File): Option[String] = {
    try {
      val fds = new FormDataSet
      val entity = new FileRepresentation(zipFile, MediaType.APPLICATION_ZIP)
      fds.getEntries.add(new FormData("AQAClient1", entity))
      fds.setMultipart(true)

      val clientResource = new ClientResource(procedure.URL)
      val challengeResponse = new ChallengeResponse(ChallengeScheme.HTTP_BASIC, ClientConfig.AQAUser, ClientConfig.AQAPassword)
      clientResource.setChallengeResponse(challengeResponse)
      val representation = clientResource.post(fds, MediaType.MULTIPART_FORM_DATA)
      val text = representation.getText
      if (text == null) None else Some("Error reported from AQA: " + text)
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error while using HTTPS client to upload zip file to AQA: " + fmtEx(t))
        // TODO should mark this image set as "problematic/error" so they don't keep getting retried.
        Some("Error communicating with AQA: " + fmtEx(t))
      }
    }
  }

  /**
   * Upload a set of DICOM files for processing.
   */
  private def upload(uploadSet: UploadSet): Unit = {
    logger.info("Processing upload " + uploadSet)
    try {
      val allDicomFiles = uploadSet.getAllDicomFiles // gather DICOM files from all series
      val zipFile = makeZipFile(allDicomFiles)
      val msg = uploadToAQA(uploadSet.procedure, zipFile)
      Sent.add(new Sent(uploadSet, msg))
      if (msg.isEmpty)
        logger.info("Successfully uploaded " + uploadSet)
      else {
        logger.warn("Failure while uploading " + uploadSet + " : " + msg.get)
      }
      Series.remove(uploadSet.imageSeries)
      if (uploadSet.reg.isDefined) Series.remove(uploadSet.reg.get)
      if (uploadSet.plan.isDefined) Series.remove(uploadSet.plan.get)
      Results.markAsStale(uploadSet.imageSeries.PatientID)

      Thread.sleep((ClientConfig.GracePeriod_sec * 1000).toLong)
      Series.removeObsoleteZipFiles // clean up any zip files
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected exception during upload: " + fmtEx(t))
      }
    }
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
        case (Some(rtplan), _) => Some(new UploadSet(Procedure.BBbyCBCT, ct, Some(rtplan))) // upload CT and RTPLAN
        case (_, true) => Some(new UploadSet(Procedure.BBbyCBCT, ct)) // upload just the CT.  The RTPLAN has already been uploaded
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
      val regOpt = Series.getRegByRegFrameOfReference(ct.FrameOfReferenceUID.get)
      if (regOpt.isDefined) {
        val reg = regOpt.get
        val localPlan = Series.getRtplanByFrameOfReference(reg.FrameOfReferenceUID.get) // if there is a copy of the plan in <code>Series</code>
        val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, reg.FrameOfReferenceUID.get) // if the plan is on the server

        (localPlan, remotePlan) match {
          case (Some(rtplan), _) => Some(new UploadSet(Procedure.BBbyCBCT, ct, Some(reg), Some(rtplan))) // upload CT, REG, and RTPLAN
          case (_, true) => Some(new UploadSet(Procedure.BBbyCBCT, ct, Some(reg))) // upload just the CT and REG.  The RTPLAN has already been uploaded
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
  private def getRtimageProcedure(rtimage: Series): Procedure = {
    val procByResult: Option[Procedure] = {
      if (rtimage.ReferencedRtplanUID.isDefined) {
        val proc = Results.getProcedureOfSeries(rtimage.PatientID, rtimage.ReferencedRtplanUID.get)
        // if DailyQACT, then it is DailyQARTIMAGE
        if (proc.isDefined && proc.get.isBBbyEPID)
          Some(Procedure.BBbyEPID)
        else
          proc
      } else
        None
    }

    if (procByResult.isDefined)
      procByResult.get
    else {
      if (rtimage.dir.list.size > 8) Procedure.Phase2 else Procedure.BBbyEPID
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
   * Given an image series, try to make an upload set.  First try CT with same frame of
   * reference as plan.  If that fails, try CT with REG file with same frame of reference as
   * plan.  If that fails, try RTIMAGE.
   */
  private def seriesToUploadSet(series: Series): Option[UploadSet] = {
    connectWithPlanbyFrameOfRef(series) match {
      case Some(uploadSet) => Some(uploadSet)
      case _ => {
        connectWithPlanViaReg(series) match {
          case Some(uploadSet) => Some(uploadSet)
          case _ => uploadableRtimage(series)
        }
      }
    }
  }

  /**
   * Look for test-ready sets of series and upload them.  Search in time-order, trying the
   * oldest sets first.
   */
  private def findSetToUploadSet: Option[UploadSet] = {
    // list of all available image series, sorted by acquisition date, and not already sent
    val list = (Series.getByModality(ModalityEnum.CT) ++ Series.getByModality(ModalityEnum.RTIMAGE)).sortBy(_.dataDate).filterNot(ser => Sent.hasImageSeries(ser.SeriesInstanceUID))

    def trySeries(seriesList: Seq[Series]): Option[UploadSet] = {
      if (seriesList.isEmpty) None
      else {
        seriesToUploadSet(seriesList.head) match {
          case Some(uploadSet) => Some(uploadSet)
          case _ => trySeries(seriesList.tail)
        }
      }
    }

    val us = trySeries(list)
    us
  }

  /**
   * Semaphore for maintaining atomicity of update function.
   */
  private val updateSync = ""

  /**
   * Look for sets of DICOM series that can be uploaded, and then upload them.
   */
  private def update: Unit = updateSync.synchronized {
    findSetToUploadSet match {
      case Some(uploadSet) => {
        // upload this one and try for another
        upload(uploadSet)
        update
      }
      case _ => ; // None to upload.  All done.
    }
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
