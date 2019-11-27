package org.aqaclient

import edu.umro.ScalaUtil.DicomCFind
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
    val newDir = new File(ClientConfig.tmpDir, SeriesInstanceUID)
    newDir.mkdirs
    val oldDir = fileList.head.getParentFile
    oldDir.renameTo(newDir)
  }

  /**
   * Get all files for the given series via C-MOVE and put them in a single directory.
   */
  private def getSeries(SeriesInstanceUID: String): Unit = {
    try {
      val fileList = DicomMove.get(SeriesInstanceUID) // fetch from DICOM source
      logger.info("For series " + SeriesInstanceUID + " " + fileList.size + " files were received")
      if (fileList.isEmpty)
        None
      else {
        val result = ClientUtil.readDicomFile(fileList.head)
        val al = ClientUtil.readDicomFile(fileList.head).right.get // get one attribute list for creating the Series
        val modality = al.get(TagFromName.Modality).getSingleStringValueOrEmptyString
        val PatientID = al.get(TagFromName.PatientID).getSingleStringValueOrEmptyString
        logger.info("For series " + SeriesInstanceUID + " " + fileList.size +
          " files were received with modality " + modality + " PatientID: " + PatientID)
        Series.put(new Series(al))
      }
    } catch {
      case t: Throwable => {
        logger.error("Unexpected error processing series: " + fmtEx(t))
      }
    }
  }

  /**
   * Get all the series for the given modality and patient, ignore those that for which there are
   * already results, and send the new series to the uploader.
   */
  private def fetchDicomOfModality(Modality: String, PatientID: String) = {
    // extract serial UIDs from DICOM C-FIND results
    val serUidList = DicomFind.find(Modality, PatientID).map(fal => ClientUtil.getSerUid(fal)).flatten
    val newSerUidList = serUidList.filter(serUid => needToGet(PatientID, serUid))
    newSerUidList.map(serUid => getSeries(serUid))
  }

  /**
   * Look for new files to process.  It is important to process CT series before
   * RTIMAGE because RTIMAGEs are dependent on the data from CTs.
   */
  private def updatePatient(PatientID: String) = {
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

  private def update = updateSync.synchronized {
    PatientIDList.getPatientIDList.map(patientID => updatePatient(patientID))
  }

  /**
   * On startup, look for files from the previous run and put them in the Series pool.
   */
  private def restoreSavedFiles = {
    def restoreSaved(dir: File) = {
      try {
        val series = new Series(ClientUtil.readDicomFile(dir.listFiles.head).right.get)
        Series.put(series)
        //logger.info("Restored series from previous run: " + series)
      } catch {
        case t: Throwable => {
          logger.warn("Unexpected error while getting DICOM series from " + dir.getAbsolutePath + " (deleting) : " + fmtEx(t))
          Utility.deleteFileTree(dir)
        }
      }
    }

    // list of files from when service was last running
    val list = ClientConfig.tmpDir.listFiles.filter(d => d.isDirectory && (!d.getName.equals(DicomMove.activeDirName)))
    list.map(dir => restoreSaved(dir))

    logger.info("Restored " + Series.size + " series from local cache.")
  }

  /**
   * If polling has been configured, then start a thread that updates regularly.
   */
  private def poll = {
    if (ClientConfig.PollInterval_sec > 0) {
      class Poll extends Runnable {
        def run = {
          while (true) {
            Thread.sleep(ClientConfig.PollInterval_sec * 1000)
            update
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
    restoreSavedFiles
    poll
    eventListener
    cullSeries
    update
  }
}
