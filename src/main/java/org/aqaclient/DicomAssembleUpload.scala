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
     *            describes the error.
     */
    override def postProcess(msg: Option[String]): Unit = {
      logger.info("performing post-processing: " + msg)
      Results.refreshPatient(this.imageSeries.PatientID)
    }
  }

  private def procedureOfCt(localPlan: Option[Series], remotePlanProcedure: Option[Procedure], ct: Series): Option[Procedure] = {

    val byRemotePlan: Seq[Procedure] = Seq(remotePlanProcedure).flatten

    val byLocalPlan: Seq[Procedure] = {
      if (localPlan.isDefined)
        localPlan.get.SOPInstanceUIDList.flatMap(procedureByReferencedRtplan)
      else
        Seq()
    }

    val byPatientId = Seq(PatientProcedure.getProcedureOfSeriesByPatientID(ct)).flatten

    val all = byRemotePlan ++ byLocalPlan ++ byPatientId
    val first = {
      all.headOption match {
        case Some(proc) if proc.isBBbyEPID => Procedure.fetchList().find(_.isBBbyCBCT)
        case Some(proc) => Some(proc)
        case _ => None
      }
    }

    first
  }

  private def dailyQaToBbByCBCT(procedure: Procedure): Procedure = {
    val p = if (procedure.isBBbyEPID) Procedure.fetchList().find(_.isBBbyCBCT).get else procedure
    p
  }

  /** Return true if the procedure is defined and references one the Daily QA OBI procedures. */
  private def usesCT(procedure: Option[Procedure]): Boolean =
    procedure.isDefined && (procedure.get.isBBbyCBCT || procedure.get.isBBbyEPID)

  /**
   * If the given the frame of reference of the CT matches an RTPLAN, then upload it.
   */
  private def connectWithPlanByFrameOfRef(ct: Series): Option[UploadSetDicomCMove] = {
    if (ct.isModality(ModalityEnum.CT)) {
      // if the server has an rtplan that connects by frame of reference, then this is the procedure of that rtplan
      val remotePlanProcedure = Results.procedureOfPlanWithFrameOfReferenceUID(ct.PatientID, ct.FrameOfReferenceUID.get)

      val localPlan = Series.getRtplanByFrameOfReference(ct.FrameOfReferenceUID.get, ct.dataDate)

      val procedureOfSeries = procedureOfCt(localPlan, remotePlanProcedure, ct) // PatientProcedure.getProcedureOfSeriesByPatientID(ct)

      0 match {
        case _ if usesCT(remotePlanProcedure) =>
          val description = ct.PatientID + " CT only, no REG"
          // upload just the CT.  The RTPLAN has already been uploaded and the REG is not needed.
          val us = new UploadSetDicomCMove(dailyQaToBbByCBCT(remotePlanProcedure.get), description, ct, reg = None, plan = None)
          Some(us)

        case _ if localPlan.isDefined && usesCT(procedureOfSeries) =>
          val description = ct.PatientID + " CT and RTPLAN, no REG"
          val us = new UploadSetDicomCMove(dailyQaToBbByCBCT(procedureOfSeries.get), description, ct, reg = None, localPlan)
          Some(us)

        case _ => None // no plan available that has the same frame of reference as this CT
      }
    } else
      None
  }

  /**
   * If there is a CT-REG pair that connect to an RTPLAN, then upload it.  Only upload the RTPLAN as necessary.
   */
  private def connectCTWithPlanViaReg(ct: Series): Option[UploadSetDicomCMove] = {

    val regOpt = Series.getRegByRegFrameOfReference(ct.FrameOfReferenceUID.get).headOption
    if (ct.isModality(ModalityEnum.CT) && regOpt.isDefined) {
      // Get the REG file that has the same frame of reference as the image file and references the image series.
      val reg = regOpt.get
      val localPlan = Series.getRtplanByFrameOfReference(reg.FrameOfReferenceUID.get, ct.dataDate) // if there is a copy of the plan in <code>Series</code>
      val remotePlanProcedure = Results.procedureOfPlanWithFrameOfReferenceUID(ct.PatientID, reg.FrameOfReferenceUID.get)

      /*
      (remotePlanProcedure, localPlan) match {
        case (Some(procedure), _) => // upload just the CT and REG.  The RTPLAN has already been uploaded
          val description = ct.PatientID + " CT and REG no RTPLAN"
          val us = new UploadSetDicomCMove(dailyQaToBbByCBCT(procedure), description, ct, Some(reg))
          Some(us)

        case (_, Some(rtplan)) if PatientProcedure.getProcedureOfSeriesByPatientID(ct).isDefined => // Upload CT, REG, and RTPLAN.
          // Base the procedure on the patient ID.
          val proc = PatientProcedure.getProcedureOfSeriesByPatientID(ct).get
          val us = new UploadSetDicomCMove(dailyQaToBbByCBCT(proc), ct.PatientID + " CT, REG, and RTPLAN", ct, Some(reg), Some(rtplan))
          Some(us)

        case _ => None
      }
      */

      0 match {
        case _ if usesCT(remotePlanProcedure) => // upload just the CT and REG.  The RTPLAN has already been uploaded
          val description = ct.PatientID + " CT and REG, no RTPLAN"
          val us = new UploadSetDicomCMove(dailyQaToBbByCBCT(remotePlanProcedure.get), description, ct, Some(reg))
          Some(us)

        case _ if localPlan.isDefined && usesCT(PatientProcedure.getProcedureOfSeriesByPatientID(ct)) => // Upload CT, REG, and RTPLAN.
          // Base the procedure on the patient ID.
          val proc = PatientProcedure.getProcedureOfSeriesByPatientID(ct).get
          val us = new UploadSetDicomCMove(dailyQaToBbByCBCT(proc), ct.PatientID + " CT, REG, and RTPLAN", ct, Some(reg), Some(localPlan.get))
          Some(us)

        case _ => None
      }

    } else
      None
  }

  private def procedureByReferencedRtplan(rtplanUID: String): Option[Procedure] = {
    Results.findRtplanBySopInstanceUid(rtplanUID) match {
      case Some(node) => Results.procedureOfNode(node)
      case _ => None
    }
  }

  private def procedureByOtherRtimageReferencingSameRtplan(rtplanUID: String): Option[Procedure] = {
    Results.findRtimageByRtplanReference(rtplanUID) match {
      case Some(node) => Results.procedureOfNode(node)
      case _ => None
    }
  }

  /**
   * Given an RTIMAGE series, determine which procedure should be run on it.  Try these in the given order:
   *
   * <ul>
   * <li>If the RTPLAN it is associated with is in turn associated with a procedure, then use that procedure.</li>
   * <li>If another RTIMAGE set references the same RTPLAN, then use the same procedure as the other RTIMAGE.</li>
   * <li>If the patient ID is associated with a procedure, then use that procedure.</li>
   * </ul>
   *
   * @param rtimage RTIMAGE series.
   * @return Procedure, if it can be determined.
   */
  private def determineProcedureForRtimageSeries(rtimage: Series): Option[Procedure] = {

    val byRtplan = {
      val p: Option[Procedure] = {
        if (rtimage.ReferencedRtplanUID.isDefined)
          procedureByReferencedRtplan(rtimage.ReferencedRtplanUID.get)
        else {
          logger.info(s"No RTPLAN ref for series modality ${rtimage.Modality}     series: ${rtimage.SeriesInstanceUID}")
          None
        }
      }
      // if the procedure is BBbyCBCT, then for RTIMAGE it means BBbyEPID
      if (p.isDefined && p.get.isBBbyCBCT) {
        val epid = Procedure.fetchList().find(_.isBBbyEPID)
        epid
      } else
        p
    }

    val byOtherRtimage = {
      if (rtimage.ReferencedRtplanUID.isDefined)
        procedureByOtherRtimageReferencingSameRtplan(rtimage.ReferencedRtplanUID.get)
      else
        None
    }
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

        //if (rtimage.dir.isDirectory)
        if (FileUtil.listFiles(rtimage.dir).isEmpty)
          Series.update(rtimage.SeriesInstanceUID, rtimage.PatientID, rtimage.Modality.toString)

        if (procedure.isDefined && FileUtil.listFiles(rtimage.dir).nonEmpty) {
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

      // list of all available image series, not have failed before, sorted by acquisition date, and not already sent
      val list = (Series.getByModality(ModalityEnum.CT) ++ Series.getByModality(ModalityEnum.RTIMAGE))
        .filterNot(series => Results.containsSeries(series))
        .filterNot(series => FailedSeries.contains(series.SeriesInstanceUID))
        .sortBy(_.dataDate)
        .filterNot(series => Sent.hasImageSeries(series.SeriesInstanceUID))
        .filter(_.isViable)

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
