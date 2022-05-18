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

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging

/**
  * Group series into sets of data that can be processed and uploaded to the AQA platform.
  */

object DicomAssembleUpload extends Logging {

  // @formatter:off
  class UploadSetDicomCMove(
    override val procedure: Procedure,
    override val description: String,
    val imageSeries: Series,
    val reg: Option[Series] = None,
    val plan: Option[Series] = None)
      extends
        UploadSet(procedure,
          description + " image series: " + imageSeries,
          ClientUtil.makeZipFile(
            fileList    = Seq(Some(imageSeries), reg, plan).flatten.flatMap(s => ClientUtil.listFiles(s.dir)),
            description = procedure.Name + "-" + procedure.Version + "_" + imageSeries.PatientID + "_" + imageSeries.Modality + "_" +  FileUtil.listFiles(imageSeries.dir).size
          )) // convert all defined series into a zip file
  // @formatter:on
  {

    /**
      * Mark the series as sent, and get the latest list of processed series for the given patient.
      *
      * @param msg Empty if upload was successful.  This is independent o the execution of the procedure, which
      *            may subsequently pass, fail, crash, timeout, or whatever.  If nonEmpty, then the message
      *             describes the error.
      */
    override def postProcess(msg: Option[String]): Unit = {
      logger.info("performing post-processing: " + msg)
      Results.refreshPatient(this.imageSeries.PatientID)
    }
  }

  /**
    * If the given CT's frame of reference matches an RTPLAN, then upload it.
    */
  private def connectWithPlanByFrameOfRef(ct: Series): Option[UploadSetDicomCMove] = {
    if (ct.isModality(ModalityEnum.CT)) {
      val localPlan = Series.getRtplanByFrameOfReference(ct.FrameOfReferenceUID.get, ct.dataDate)
      val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, ct.FrameOfReferenceUID.get)

      val procedureOfSeries = PatientProcedure.getProcedureOfSeriesByPatientID(ct)

      (localPlan, remotePlan) match {
        // upload CT and RTPLAN
        case (Some(rtplan), _) if procedureOfSeries.isDefined => Some(new UploadSetDicomCMove(procedureOfSeries.get, ct.PatientID + " CT and RTPLAN", ct, reg = None, Some(rtplan)))

        // upload just the CT.  The RTPLAN has already been uploaded
        case (_, true) if procedureOfSeries.isDefined => Some(new UploadSetDicomCMove(procedureOfSeries.get, ct.PatientID + " CT only", ct))

        case _ => None // no plan available that has the same frame of reference as this CT
      }
    } else
      None
  }

  /**
    * If there is a CT-REG pair that connect to an RTPLAN, then upload it.  Only upload the RTPLAN as necessary.
    */
  private def connectCTWithPlanViaReg(ct: Series): Option[UploadSetDicomCMove] = {
    val bbByCBCTProcedure = Procedure.fetchList().find(_.isBBbyCBCT) // the server must support DailyQA OBI
    val regOpt = Series.getRegByRegFrameOfReference(ct.FrameOfReferenceUID.get).headOption //  .filter(regRefsImage).headOption
    if (ct.isModality(ModalityEnum.CT) && bbByCBCTProcedure.isDefined && regOpt.isDefined) {
      // Get the REG file that has the same frame of reference as the image file and references the image series.
      val reg = regOpt.get
      val localPlan = Series.getRtplanByFrameOfReference(reg.FrameOfReferenceUID.get, ct.dataDate) // if there is a copy of the plan in <code>Series</code>
      val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, reg.FrameOfReferenceUID.get) // if the plan is on the server

      (localPlan, remotePlan) match {
        case (Some(rtplan), _) =>
          Some(new UploadSetDicomCMove(bbByCBCTProcedure.get, ct.PatientID + " CT, REG, and RTPLAN", ct, Some(reg), Some(rtplan))) // upload CT, REG, and RTPLAN
        case (_, true) =>
          Some(new UploadSetDicomCMove(bbByCBCTProcedure.get, ct.PatientID + " CT and REG, no RTPLAN", ct, Some(reg))) // upload just the CT and REG.  The RTPLAN has already been uploaded
        case _ => None
      }
    } else
      None
  }

  private def procedureByReferencedRtplan(rtplanUID: String): Option[Procedure] = {
    Results.findRtplanBySopInstanceUid(rtplanUID) match {
      case Some(node) => Results.procedureOfNode(node)
      case _          => None
    }
  }

  private def procedureByOtherRtimageReferencingSameRtplan(rtplanUID: String): Option[Procedure] = {
    Results.findRtimageByRtplanReference(rtplanUID) match {
      case Some(node) => Results.procedureOfNode(node)
      case _          => None
    }
  }

  /**
    * Given an RTIMAGE series, determine which procedure should be run on it.  Try these in the given order:
    *
    *  <ul>
    *    <li>If the RTPLAN it is associated with is in turn associated with a procedure, then use that procedure.</li>
    *    <li>If another RTIMAGE set references the same RTPLAN, then use the same procedure as the other RTIMAGE.</li>
    *    <li>If the patient ID is associated with a procedure, then use that procedure.</li>
    *  </ul>
    *
    * @param rtimage RTIMAGE series.
    * @return Procedure, if it can be determined.
    */
  private def determineProcedureForRtimageSeries(rtimage: Series): Option[Procedure] = {

    val byRtplan = procedureByReferencedRtplan(rtimage.ReferencedRtplanUID.get)
    val byOtherRtimage = procedureByOtherRtimageReferencingSameRtplan(rtimage.ReferencedRtplanUID.get)
    val byPatientId = PatientProcedure.getProcedureOfSeriesByPatientID(rtimage)

    val procedureList: Seq[Option[Procedure]] = Seq(
      byRtplan,
      byOtherRtimage,
      byPatientId
    )

    val procedure = procedureList.flatten.headOption

    procedure
  }

  /**
    * If the RTPLAN referenced by the given RTIMAGE series is on the client and is not on the
    * server, then return it.  Else return None.
    *
    * @param rtimage RTPLAN for this RTIMAGE.
    * @return RTPLAN series, if it needs to be uploaded.
    */
  private def uploadRtplanOfRtimageIfNeeded(rtimage: Series): Option[Series] = {
    val rtplanSopInstanceUid = rtimage.ReferencedRtplanUID

    rtplanSopInstanceUid match {
      // true if RTIMAGE does not reference an RTPLAN
      case None =>
        logger.error("RTIMAGE file does not reference an RTPLAN: " + rtimage)
        None // RTIMAGE does not reference an RTPLAN.  This should never happen.

      // true if RTPLAN is already on the server
      case Some(planRef) if Results.findRtplanBySopInstanceUid(rtplanInstanceUid = planRef).isDefined =>
        None

      // if the client has it, then return it
      case _ =>
        val rtplanSeries = Series.getByModality(ModalityEnum.RTPLAN).find(s => s.SOPInstanceUIDList.contains(rtplanSopInstanceUid.get))
        rtplanSeries // if defined return it, else return None
    }
  }

  /**
    * Make UploadSet from RTIMAGE series.
    */
  private def uploadableRtimage(rtimage: Series): Option[UploadSetDicomCMove] = {
    val uploadSet: Option[UploadSetDicomCMove] = {
      if (rtimage.isModality(ModalityEnum.RTIMAGE)) {
        val procedure = determineProcedureForRtimageSeries(rtimage)
        if (procedure.isDefined) {
          val rtplan = uploadRtplanOfRtimageIfNeeded(rtimage) // only defined if we have it and the server does not
          Some(
            new UploadSetDicomCMove(
              procedure = procedure.get,
              description = rtimage.PatientID + " RTIMAGE only",
              imageSeries = rtimage,
              reg = None,
              plan = rtplan
            )
          )
        } else
          None // procedure not defined
      } else {
        None // this is not an RTIMAGE series
      }
    }

    uploadSet
  }

  /**
    * Given an image series, try to make an upload set.  First try CT with same frame of
    * reference as plan.  If that fails, try CT with REG file with same frame of reference as
    * plan.  If that fails, try RTIMAGE.
    *
    * @param series Image series.
    *
    * @return Either the set of files to upload, or None if the required files can not be found.
    */
  private def seriesToUploadSet(series: Series): Option[UploadSetDicomCMove] = {
    val uploadSet = connectWithPlanByFrameOfRef(series) match {
      case Some(uploadSet) =>
        Some(uploadSet)
      case _ =>
        connectCTWithPlanViaReg(series) match {
          case Some(uploadSet) =>
            Some(uploadSet)
          case _ =>
            uploadableRtimage(series)
        }
    }
    uploadSet
  }

  /**
    * Semaphore for maintaining atomicity of update function.
    */
  private val updateSyncDicomAssembleUpload = "updateSyncDicomAssembleUpload"

  /**
    * Look for sets of DICOM series that can be uploaded, and then upload them.
    */
  private def update(): Unit =
    updateSyncDicomAssembleUpload.synchronized {
      // ignore image series that are too old
      val recent = System.currentTimeMillis - ClientConfig.MaximumDataAge_ms

      // list of all available image series, not have failed before, sorted by acquisition date, and not already sent
      val list = (Series.getByModality(ModalityEnum.CT) ++ Series.getByModality(ModalityEnum.RTIMAGE))
        .filterNot(series => Results.containsSeries(series))
        .filterNot(series => FailedSeries.contains(series.SeriesInstanceUID))
        .sortBy(_.dataDate)
        .filterNot(series => Sent.hasImageSeries(series.SeriesInstanceUID))
        .filter(_.dataDate.getTime > recent)

      val todoList = list.flatMap(series => seriesToUploadSet(series))
      todoList.foreach(uploadSet => {
        logger.info("Queueing upload set: " + uploadSet)
        Sent.add(new Sent(uploadSet, Some(uploadSet.toString)))
        Upload.put(uploadSet)
      })
      todoList.foreach(uploadSet => ConfirmDicomComplete.confirmDicomComplete(uploadSet))
    }

  /**
    * Indicate that new data is available for processing.  There may or may not be a set that can be
    * processed.  This function sends a message to the Upload thread and returns immediately.
    */
  def scanSeries(): Unit = {
    update()
  }

  def init(): Unit = {
    logger.info("initializing Upload")
    // startUpdateThread()
    logger.info("Upload has been initialized")
  }

}
