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
  * Orchestrate all C-FIND and C-MOVE operations to obtain the relevant DICOM.  It is
  * sent via FIFO queue to another thread that groups it into sets for testing.
  */
object DicomProcessing extends Logging {

  /**
    * Get all files for the given series via C-MOVE.
    */
  private def fetchSeries(SeriesInstanceUID: String, PatientID: String, Modality: String): Unit = {
    DicomMove.get(SeriesInstanceUID, PatientID, Modality) match {
      case Some(series) =>
        Series.persist(series)
        if (series.isViable) DicomAssembleUpload.scanSeries()
      case _ => ;
    }
  }

  /**
    * Get all the series for the given modality and patient and send the new series to the
    * uploader.  Ignore series that are marked as failed or have already been gotten or those
    * that are known by the server.
    */
  private def fetchDicomOfModality(Modality: String, PatientID: String): Unit = {
    val serUidList = DicomFind.find(Modality, PatientID).flatMap(fal => ClientUtil.getSerUid(fal))
    val newSerUidList = serUidList.filterNot(serUid => FailedSeries.contains(serUid)).filterNot(serUid => Series.contains(serUid)).filterNot(serUid => Results.containsSeries(PatientID, serUid))
    newSerUidList.foreach(serUid => fetchSeries(serUid, PatientID, Modality))
  }

  /**
    * Look for new files to process.  It is important to process CT series before
    * RTIMAGE because each RTIMAGE is dependent on the data from CTs.
    */
  def updatePatient(patientProcedure: PatientProcedure): Unit = {

    // List of all modalities that should be fetched for this patient.  Sorted only so that
    // they will be used in a consistent way.
    val modalityList = patientProcedure.procedureList.flatMap(_.modalityList).distinct.sorted
    logger.info("Updating patient ID: " + patientProcedure.patientId)
    modalityList.foreach(Modality => fetchDicomOfModality(Modality, patientProcedure.patientId))
  }

  private def update(): Unit = {
    logger.info("Updating DICOM for " + PatientProcedure.patientIdList.size + " patient IDs.")
    PatientProcedure.getPatientProcedureList.foreach(updatePatient)
  }

  /**
    * If polling has been configured, then start a thread that updates regularly.
    */
  private def poll(): Unit = {
    if (ClientConfig.PollInterval_sec > 0) {
      class Poll extends Runnable {
        def run(): Unit = {
          while (true) {
            update()
            Thread.sleep(ClientConfig.PollInterval_sec * 1000)
          }
        }
      }
      new Thread(new Poll).start()
    }
  }

  private def eventListener(): Unit = {
    logger.info("Need to write this")
  }

  def init(): Unit = {
    logger.info("initializing DicomProcessing")
    //restoreSavedFiles
    poll()
    eventListener()
    //    cullSeries
  }
}
