package org.aqaclient

import edu.umro.ScalaUtil.Logging
import edu.umro.util.Utility

/**
 * Orchestrate all C-FIND and C-MOVE operations to obtain the relevant DICOM.  It is
 * sent via FIFO queue to another thread that groups it into sets for testing.
 */
object DicomProcessing extends Logging {


  /**
   * Get all files for the given series via C-MOVE.
   */
  private def fetchSeries(SeriesInstanceUID: String, description: String): Unit = {
    DicomMove.get(SeriesInstanceUID, description) match {
      case Some(series) =>
        Series.persist(series)
        if (series.isViable) Upload.scanSeries
      case _ => ;
    }
  }

  /**
   * Get all the series for the given modality and patient and send the new series to the
   * uploader.  Ignore series that are marked as failed or have already been gotten or those
   * that are known by the server.
   */
  private def fetchDicomOfModality(Modality: String, PatientID: String) = {
    val serUidList = DicomFind.find(Modality, PatientID).flatMap(fal => ClientUtil.getSerUid(fal))
    val newSerUidList = serUidList.
      filterNot(serUid => FailedSeries.contains(serUid)).
      filterNot(serUid => Series.contains(serUid)).
      filterNot(serUid => Results.containsSeries(PatientID, serUid))
    newSerUidList.map(serUid => fetchSeries(serUid, PatientID + " : " + Modality))
  }

  /**
   * Look for new files to process.  It is important to process CT series before
   * RTIMAGE because RTIMAGEs are dependent on the data from CTs.
   */
  def updatePatient(PatientID: String): Seq[Seq[Unit]] = updateSync.synchronized {
    Seq("RTPLAN", "REG", "CT", "RTIMAGE").map(Modality => fetchDicomOfModality(Modality, PatientID))
  }

  /**
   * Use this as a semaphore to only permit one update to be executed at a time.
   */
  private val updateSync = 0

  private def update = {
    logger.info("Getting updated list of DICOM files for patients IDs:    " +
      PatientIDList.getPatientIDList.mkString("    "))
    PatientIDList.getPatientIDList.map(patientID => updatePatient(patientID))
  }

  /**
   * Remove temporary files if there are any.
   */
  private def cleanup = {
    if (DicomMove.activeDir.exists)
      try {
        Utility.deleteFileTree(DicomMove.activeDir)
      } catch {
        case _: Throwable => ;
      }

    ClientConfig.seriesDir.listFiles.filter(f => f.getName.toLowerCase.endsWith(".tmp")).map(f => f.delete)
  }

  /**
   * If polling has been configured, then start a thread that updates regularly.
   */
  private def poll(): Unit = {
    if (ClientConfig.PollInterval_sec > 0) {
      class Poll extends Runnable {
        def run(): Unit = {
          while (true) {
            update
            Thread.sleep(ClientConfig.PollInterval_sec * 1000)
          }
        }
      }
      new Thread(new Poll).start()
    }
  }

  private def eventListener(): Unit = {
    logger.info("Need to write this") // TODO
  }

  def init(): Unit = {
    logger.info("initializing DicomProcessing")
    cleanup
    //restoreSavedFiles
    poll()
    eventListener()
    //    cullSeries
  }
}
