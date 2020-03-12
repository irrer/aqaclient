package org.aqaclient

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.DicomUtil
import java.io.File
import edu.umro.util.Utility

/**
 * Orchestrate all C-FIND and C-MOVE operations to obtain the relevant DICOM.  It is
 * sent via FIFO queue to another thread that groups it into sets for testing.
 */
object DicomProcessing extends Logging {

  /**
   * Return true if the series needs to be retrieved.
   */
  private def needToGet(PatientID: String, SeriesInstanceUID: String): Boolean = {
    (!Results.containsSeries(PatientID, SeriesInstanceUID)) && (!Series.contains(SeriesInstanceUID))
  }

  /**
   * Move the temporary directory to the permanent directory.
   */
  private def moveSeriesFiles(fileList: List[File], SeriesInstanceUID: String) = {
    val newDir = new File(ClientConfig.seriesDir, SeriesInstanceUID)
    newDir.mkdirs
    val oldDir = fileList.head.getParentFile
    oldDir.renameTo(newDir)
  }

  /**
   * Get all files for the given series via C-MOVE.
   */
  private def getSeries(SeriesInstanceUID: String, description: String): Unit = {
    DicomMove.get(SeriesInstanceUID, description) match {
      case Some(series) => Series.put(series)
      case _ => ;
    }
  }

  /**
   * Get all the series for the given modality and patient, ignore those that for which there are
   * already results, and send the new series to the uploader.
   */
  private def fetchDicomOfModality(Modality: String, PatientID: String) = {
    // extract serial UIDs from DICOM C-FIND results
    val serUidList = DicomFind.find(Modality, PatientID).map(fal => ClientUtil.getSerUid(fal)).flatten
    val newSerUidList = serUidList.filter(serUid => needToGet(PatientID, serUid)).filterNot(serUid => FailedSeries.contains(serUid))
    newSerUidList.map(serUid => getSeries(serUid, PatientID + " : " + Modality))
  }

  /**
   * Look for new files to process.  It is important to process CT series before
   * RTIMAGE because RTIMAGEs are dependent on the data from CTs.
   */
  def updatePatient(PatientID: String) = updateSync.synchronized {
    Seq("RTPLAN", "REG", "CT", "RTIMAGE").map(Modality => fetchDicomOfModality(Modality, PatientID))
  }

  /**
   * Use this as a semaphore to only permit one update to be executed at a time.
   */
  private val updateSync = 0

  /**
   * Remove any series that have been processed.
   */
  def cullSeries = {
    val toRemove = Series.getAllSeries.filter(ser => Results.containsSeries(ser.PatientID, ser.SeriesInstanceUID))
    logger.info("Found " + toRemove.size + " locally cached series to remove.")
    toRemove.map(ser => Series.remove(ser))
  }

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
        case t: Throwable => ;
      }

    ClientConfig.seriesDir.listFiles.filter(f => f.getName.toLowerCase.endsWith(".tmp")).map(f => f.delete)
  }

  /**
   * If polling has been configured, then start a thread that updates regularly.
   */
  private def poll = {
    if (ClientConfig.PollInterval_sec > 0) {
      class Poll extends Runnable {
        def run = {
          while (true) {
            update
            Thread.sleep(ClientConfig.PollInterval_sec * 1000)
          }
        }
      }
      (new Thread(new Poll)).start
    }
  }

  private def eventListener = {
    logger.info("Need to write this") // TODO
  }

  def init = {
    logger.info("initializing DicomProcessing")
    cleanup
    //restoreSavedFiles
    poll
    eventListener
    cullSeries
  }
}
