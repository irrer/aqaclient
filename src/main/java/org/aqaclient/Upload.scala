package org.aqaclient

import edu.umro.ScalaUtil.Logging
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
import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeList

/**
 * Group series into sets of data that can be processed and upload to the AQA platform.
 */

object Upload extends Logging {

  /**
   * Semaphore for maintaining atomicity of update function.
   */
  private val updateSync = ""

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
      procedure.toString + "  " + imageSeries.toString + "    PatientID: " + imageSeries.PatientID + r + "    " + p
    }

    /**
     * Get a list of all files in this upload set.
     */
    def getAllDicomFiles: Seq[File] = {
      def filesOf(series: Option[Series]): Seq[File] =
        if (series.isDefined) {
          series.get.ensureFilesExist
          ClientUtil.listFiles(series.get.dir).toSeq
        } else Seq[File]()

      val regFiles: Seq[File] = {
        if (reg.isDefined) {
          reg.get.ensureFilesExist
          val regFileList = ClientUtil.listFiles(reg.get.dir).toSeq
          def regRefsImage(regFile: File): Boolean = {
            val regAl = ClientUtil.readDicomFile(regFile).right.get
            val serList = DicomUtil.findAllSingle(regAl, TagFromName.SeriesInstanceUID).map(at => at.getSingleStringValueOrEmptyString)
            val contains = serList.contains(imageSeries.SeriesInstanceUID)
            contains
          }
          regFileList.filter(regFile => regRefsImage(regFile))
        } else Seq[File]()
      }

      filesOf(Some(imageSeries)) ++ regFiles ++ filesOf(plan)
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
  private def uploadToAQA(procedure: Procedure, series: Series, zipFile: File): Option[String] = {
    try {
      val start = System.currentTimeMillis
      logger.info("Starting upload of data set to AQA for procedure " + procedure.Name + "    PatientID: " + series.PatientID)
      val result = HttpsClient.httpsPostSingleFileAsMulipartForm(procedure.URL, zipFile, MediaType.APPLICATION_ZIP,
        ClientConfig.AQAUser, ClientConfig.AQAPassword, ChallengeScheme.HTTP_BASIC, true, ClientConfig.httpsClientParameters, timeout_ms = ClientConfig.HttpsUploadTimeout_ms)
      val elapsed = System.currentTimeMillis - start
      result match {
        case Right(good) => {
          logger.info("Elapsed time in ms of upload: " + elapsed + "  Successful upload of data set to AQA for procedure " + procedure.Name + "    PatientID: " + series.PatientID)
          println(good)
          None
        }
        case Left(failure) => {
          logger.warn("Elapsed time in ms of upload: " + elapsed + "  Failed to upload data set to AQA for procedure " + procedure.Name + "    PatientID: " + series.PatientID + " : " + fmtEx(failure))
          Some("Elapsed time in ms of upload: " + elapsed + "  failed to upload data set to AQA: " + failure.getMessage)
        }
      }
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
  def upload(uploadSet: UploadSet): Unit = {
    logger.info("Processing upload " + uploadSet)
    try {
      val allDicomFiles = uploadSet.getAllDicomFiles // gather DICOM files from all series
      val zipFile = makeZipFile(allDicomFiles)
      logger.info("Beginning upload of " + uploadSet)
      val msg = uploadToAQA(uploadSet.procedure, uploadSet.imageSeries, zipFile)
      logger.info("Finished upload of " + uploadSet)
      Sent.add(new Sent(uploadSet, msg))
      if (msg.isEmpty)
        logger.info("Successfully uploaded " + uploadSet)
      else {
        logger.warn("Failure while uploading " + uploadSet + " : " + msg.get)
      }
      //      Series.remove(uploadSet.imageSeries)
      //      if (uploadSet.reg.isDefined) Series.remove(uploadSet.reg.get)
      //      if (uploadSet.plan.isDefined) Series.remove(uploadSet.plan.get)
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
        case (Some(rtplan), _) if Procedure.BBbyCBCT.isDefined => Some(new UploadSet(Procedure.BBbyCBCT.get, ct, Some(rtplan))) // upload CT and RTPLAN
        case (_, true) if Procedure.BBbyCBCT.isDefined => Some(new UploadSet(Procedure.BBbyCBCT.get, ct)) // upload just the CT.  The RTPLAN has already been uploaded
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
          case (Some(rtplan), _) if Procedure.BBbyCBCT.isDefined => Some(new UploadSet(Procedure.BBbyCBCT.get, ct, Some(reg), Some(rtplan))) // upload CT, REG, and RTPLAN
          case (_, true) if Procedure.BBbyCBCT.isDefined => Some(new UploadSet(Procedure.BBbyCBCT.get, ct, Some(reg))) // upload just the CT and REG.  The RTPLAN has already been uploaded
          case _ => None
        }
      } else
        None
    } else
      None
  }

  /**
   * Determine if the given series is a Phase2 data set based on the the content.  It must have
   * a minimum number of files (currently 16), and each file must reference a different beam.
   */
  private def isPhase2(rtimage: Series): Boolean = {

    /** The data set must have at least this many files to be a Phase 2 data set. */
    val minNumberOfFiles = 16

    val fileList = FileUtil.listFiles(rtimage.dir)

    def beamReferencesAreAllUnique: Boolean = {
      val alList = fileList.map(file => ClientUtil.readDicomFile(file)).filter(_.isRight).map(_.right.get)
      val beamList = alList.map(al => al.get(TagFromName.ReferencedBeamNumber).getIntegerValues()(0)).distinct
      val isP2 = alList.size == beamList.size
      logger.info("Number of attribute lists: " + alList.size + "    Number of unique beams: " + beamList.size + "    isPhase2 data set: " + isP2)
      isP2
    }

    (fileList.size >= minNumberOfFiles) && beamReferencesAreAllUnique
  }

  /**
   * Get the procedure that this series should be processed with.  First try by looking at the plan it references to see what that
   * was processed with.  If that fails, then just look at the number of slices in the series and make an assumption.
   */
  private def getRtimageProcedure(rtimage: Series): Option[Procedure] = {
    val procByResult: Option[Procedure] = {
      if (rtimage.ReferencedRtplanUID.isDefined) {
        val proc = Results.getProcedureOfSeries(rtimage.PatientID, rtimage.ReferencedRtplanUID.get)
        // if DailyQACT, then it is DailyQARTIMAGE
        if (proc.isDefined && proc.get.isBBbyEPID)
          Procedure.BBbyEPID
        else
          proc
      } else
        None
    }

    if (procByResult.isDefined)
      procByResult
    else {
      rtimage.ensureFilesExist
      if (isPhase2(rtimage)) Procedure.Phase2 else Procedure.BBbyEPID
    }
  }

  /**
   * Make UploadSet from RTIMAGE series.
   */
  private def uploadableRtimage(rtimage: Series): Option[UploadSet] = {
    if (rtimage.isModality(ModalityEnum.RTIMAGE)) {
      val procedure = getRtimageProcedure(rtimage)
      if (procedure.isDefined) Some(new UploadSet(procedure.get, rtimage)) else None
    } else
      None
  }

  /**
   * Given an image series, try to make an upload set.  First try CT with same frame of
   * reference as plan.  If that fails, try CT with REG file with same frame of reference as
   * plan.  If that fails, try RTIMAGE.
   */
  private def seriesToUploadSet(series: Series): Option[UploadSet] = {
    val uploadSet = connectWithPlanbyFrameOfRef(series) match {
      case Some(uploadSet) => Some(uploadSet)
      case _ => {
        connectWithPlanViaReg(series) match {
          case Some(uploadSet) => Some(uploadSet)
          case _ => uploadableRtimage(series)
        }
      }
    }
    uploadSet
  }

  /**
   * Look for sets of DICOM series that can be uploaded, and then upload them.
   */
  private def update: Unit = updateSync.synchronized {
    // ignore image series that are too old
    val recent = System.currentTimeMillis - ClientConfig.MaximumDataAge_ms

    // list of all available image series, not have failed before, sorted by acquisition date, and not already sent
    val list = (Series.getByModality(ModalityEnum.CT) ++ Series.getByModality(ModalityEnum.RTIMAGE)).
      filterNot(series => Results.containsSeries(series)).
      filterNot(series => FailedSeries.contains(series.SeriesInstanceUID)).
      sortBy(_.dataDate).
      filterNot(series => Sent.hasImageSeries(series.SeriesInstanceUID))
      .filter(_.dataDate.getTime > recent)

    val todoList = list.map(series => seriesToUploadSet(series)).flatten
    todoList.map(uploadSet => upload(uploadSet))
    todoList.map(uploadSet => ConfirmDicomComplete.confirmDicomComplete(uploadSet))
  }

  private val queue = new java.util.concurrent.LinkedBlockingQueue[Boolean]

  private def startUpdateThread = {
    class Updater extends Runnable {
      def run = {
        while (true) {
          logger.info("Processing new DICOM files.  queue size: " + queue.size)
          update
          queue.take
        }
      }
    }

    (new Thread(new Updater)).start
  }

  /**
   * Indicate that new data is available for processing.  There may or may not be a set that can be
   * processed.  This function sends a message to the Upload thread and returns immediately.
   */
  def scanSeries = {
    queue.put(true)
  }

  def init = {
    logger.info("initializing Upload")
    startUpdateThread
    logger.info("Upload has been initialized")
  }

}
