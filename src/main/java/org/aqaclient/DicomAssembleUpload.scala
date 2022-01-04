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

import edu.umro.ScalaUtil.Logging

/**
  * Group series into sets of data that can be processed and upload to the AQA platform.
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
          ClientUtil.makeZipFile(Seq(Some(imageSeries), reg, plan).flatten.flatMap(s => ClientUtil.listFiles(s.dir)))) // convert all defined series into a zip file
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
      Sent.add(new Sent(this, msg))
      Results.refreshPatient(this.imageSeries.PatientID)
    }
  }

  /**
    * If the given CT's frame of reference matches an RTPLAN, then upload it.
    */
  private def connectWithPlanByFrameOfRef(ct: Series): Option[UploadSetDicomCMove] = {
    if (ct.isModality(ModalityEnum.CT)) {
      val localPlan = Series.getRtplanByFrameOfReference(ct.FrameOfReferenceUID.get)
      val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, ct.FrameOfReferenceUID.get)

      val procedureOfSeries = PatientProcedure.getProcedureOfSeries(ct)

      (localPlan, remotePlan) match {
        // upload CT and RTPLAN
        case (Some(rtplan), _) if procedureOfSeries.isDefined => Some(new UploadSetDicomCMove(procedureOfSeries.get, "CT and RTPLAN", ct, reg = None, Some(rtplan)))

        // upload just the CT.  The RTPLAN has already been uploaded
        case (_, true) if procedureOfSeries.isDefined => Some(new UploadSetDicomCMove(procedureOfSeries.get, "CT only", ct))

        case _ => None // no plan available that has the same frame of reference as this CT
      }
    } else
      None
  }

  /**
    * If there is a CT-REG pair that connect to an RTPLAN, then upload it.  Only upload the RTPLAN as necessary.
    */
  private def connectWithPlanViaReg(ct: Series): Option[UploadSetDicomCMove] = {
    if (ct.isModality(ModalityEnum.CT)) {

      // Get the REG file that has the same frame of reference as the image file and references the image series.
      val regOpt = Series.getRegByRegFrameOfReference(ct.FrameOfReferenceUID.get).headOption //  .filter(regRefsImage).headOption

      val procedureOfSeries = PatientProcedure.getProcedureOfSeries(ct)

      if (regOpt.isDefined) {
        val reg = regOpt.get
        val localPlan = Series.getRtplanByFrameOfReference(reg.FrameOfReferenceUID.get) // if there is a copy of the plan in <code>Series</code>
        val remotePlan = Results.containsPlanWithFrameOfReferenceUID(ct.PatientID, reg.FrameOfReferenceUID.get) // if the plan is on the server

        (localPlan, remotePlan) match {
          case (Some(rtplan), _) if procedureOfSeries.isDefined =>
            Some(new UploadSetDicomCMove(procedureOfSeries.get, "CT, REG, and RTPLAN", ct, Some(reg), Some(rtplan))) // upload CT, REG, and RTPLAN
          case (_, true) if procedureOfSeries.isDefined =>
            Some(new UploadSetDicomCMove(procedureOfSeries.get, "CT and REG, no RTPLAN", ct, Some(reg))) // upload just the CT and REG.  The RTPLAN has already been uploaded
          case _ => None
        }
      } else
        None
    } else
      None
  }

  /**
    * Make UploadSet from RTIMAGE series.
    */
  private def uploadableRtimage(rtimage: Series): Option[UploadSetDicomCMove] = {
    if (rtimage.isModality(ModalityEnum.RTIMAGE)) {
      val procedure = PatientProcedure.getProcedureOfSeries(rtimage)
      if (procedure.isDefined) Some(new UploadSetDicomCMove(procedure.get, "RTIMAGE only", rtimage)) else None
    } else
      None
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
        connectWithPlanViaReg(series) match {
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
  private val updateSync = ""

  /**
    * Look for sets of DICOM series that can be uploaded, and then upload them.
    */
  private def update(): Unit =
    updateSync.synchronized {
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
      todoList.foreach(uploadSet => Upload.put(uploadSet))
      todoList.map(uploadSet => ConfirmDicomComplete.confirmDicomComplete(uploadSet))
    }

  private val queue = new java.util.concurrent.LinkedBlockingQueue[Boolean]

  private def startUpdateThread(): Unit = {
    class Updater extends Runnable {
      def run(): Unit = {
        while (true) {
          logger.info("Processing new DICOM files.  queue size: " + queue.size)
          update()
          queue.take
        }
      }
    }

    new Thread(new Updater).start()
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
    startUpdateThread()
    logger.info("Upload has been initialized")
  }

}
