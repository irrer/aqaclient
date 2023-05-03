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

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.Logging

import java.util.Date

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
    val search = {
      // if (PatientID.equals("BR1_OBI_QA_2023_Q1")) "*R1_OBI_QA_2023_*" else PatientID
      ClientConfig.PatientIDCFindPatternMap.get(PatientID) match {
        case Some(pattern) => pattern
        case _             => PatientID
      }
    }

    def patientIdFilter(al: AttributeList) = (al.get(TagByName.PatientID) != null) && al.get(TagByName.PatientID).getSingleStringValueOrEmptyString().equals(PatientID)

    val serUidList = DicomFind.find(Modality, search).filter(patientIdFilter).flatMap(fal => ClientUtil.getSerUid(fal))
    val newSerUidList = serUidList.filterNot(serUid => FailedSeries.contains(serUid)).filterNot(serUid => Series.contains(serUid)).filterNot(serUid => Results.containsSeries(PatientID, serUid))
    newSerUidList.foreach(serUid => fetchSeries(serUid, PatientID, Modality))
  }

  /**
    * Get the date of the most recently created DICOM file for this patient.
    * If there are no DICOM files for the patient, then return None.
    *
    * @param patientId ID of Patient of interest.
    * @return Time in ms.
    */
  private def getMostRecentActivityDate(patientId: String): Option[Date] = {
    val list = Series.getAllSeries.filter(_.PatientID.equals(patientId))
    if (list.isEmpty)
      None
    else {
      val latest = list.maxBy(_.dataDate).dataDate
      Some(latest)
    }
  }

  /**
    * Look for new files to process.  It is important to process CT series before
    * RTIMAGE because each RTIMAGE is dependent on the data from CTs.
    */
  def updatePatient(patientProcedure: PatientProcedure): Unit = {
    val mostRecentFileDate: Date = {
      getMostRecentActivityDate(patientProcedure.patientId) match {
        case Some(t) => t
        case _       => new Date(System.currentTimeMillis() - PollInterval.maxAge_ms())
      }
    }

    val msInDay = 24 * 60 * 60 * 1000.0
    val maxFileAge = (System.currentTimeMillis() - mostRecentFileDate.getTime) / msInDay
    val poll = PollInterval.shouldBePolled(mostRecentFileDate)
    logger.info("PatientID: " + patientProcedure.patientId + "    Max File Age in Days: " + maxFileAge.formatted("%8.1f") + "    poll: " + poll)

    if (poll) {
      // List of all modalities that should be fetched for this patient.  Sorted only so that
      // they will be used in a consistent way.
      val modalityList = Seq("RTPLAN", "CT", "REG", "RTIMAGE")
      logger.info("Updating patient ID: " + patientProcedure.patientId)
      modalityList.foreach(Modality => fetchDicomOfModality(Modality, patientProcedure.patientId))
    }
  }

  private def update(): Unit = {
    try {
      logger.info("Updating DICOM for " + PatientProcedure.patientIdList.size + " patient IDs.")
      PatientProcedure.getPatientProcedureList.foreach(updatePatient)
    } catch {
      case t: Throwable =>
        logger.error(s"DicomProcessing.update: Unexpected exception/error: ${fmtEx(t)}")
    }
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
            PollInterval.updatePollTimeIfExpired()
            Thread.sleep(PollInterval.minPoll_ms())
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
