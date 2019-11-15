package org.aqaclient

import edu.umro.ScalaUtil.DicomCFind
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import edu.umro.ScalaUtil.Logging
import org.aqaclient.series.Series
import edu.umro.ScalaUtil.DicomUtil
import java.io.File
import org.aqaclient.series.SeriesRtplan
import org.aqaclient.series.SeriesReg
import org.aqaclient.series.SeriesCt
import org.aqaclient.series.SeriesRtimage
import org.aqaclient.series.SeriesReg
import edu.umro.util.Utility

/**
 * Orchestrate all C-FIND and C-MOVE operations to obtain the relevant DICOM.  It is
 * sent via FIFO queue to another thread that groups it into sets for testing.
 */
object DicomProcessing extends Logging {

  /**
   * Return true if the series needs to be retrieved.
   */
  private def needToGet(SeriesInstanceUID: String): Boolean = {
    (!ProcessedSeries.contains(SeriesInstanceUID)) && (!Series.contains(SeriesInstanceUID))
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
  private def getSeries(SeriesInstanceUID: String): Option[AttributeList] = {
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
        Some(al)
      }
    } catch {
      case t: Throwable => {
        logger.error("Unexpected error processing series: " + fmtEx(t))
        None
      }
    }
  }

  /**
   * Get all the series for the given modality and patient.
   */
  private def fetchDicomOfModality(Modality: String, PatientID: String) = {
    val serUidList = DicomFind.find(Modality, PatientID).map(fal => ClientUtil.getSerUid(fal)).flatten.filter(serUid => needToGet(serUid))
    val alList = serUidList.map(planFind => getSeries(planFind)).flatten
    alList
  }

  /**
   * Look for new files to process.  It is important to process CT series before
   * RTIMAGE because RTIMAGEs are dependent on the data from CTs.
   */
  private def updatePatient(PatientID: String) = {
    fetchDicomOfModality("RTPLAN", PatientID).map(al => Series.put(new SeriesRtplan(al)))
    fetchDicomOfModality("REG", PatientID).map(al => Series.put(new SeriesReg(al)))
    fetchDicomOfModality("CT", PatientID).map(al => Series.put(new SeriesCt(al)))
    fetchDicomOfModality("RTIMAGE", PatientID).map(al => Series.put(new SeriesRtimage(al)))
  }

  /**
   * Use this as a semaphore to only permit one update to be executed at a time.
   */
  private val updateSync = 0

  private def update = updateSync.synchronized({
    PatientIDList.getPatientIDList.map(patientID => updatePatient(patientID))
  })

  /**
   * On startup, look for files from the previous run and put them in the Series pool.
   */
  private def putSavedFiles = {
    def putSaved(dir: File) = {
      try {
        val series = Series.constructSeries(ClientUtil.readDicomFile(dir.listFiles.head).right.get)
        Series.put(series)
        logger.info("Restored series from previous run: " + series)
      } catch {
        case t: Throwable => {
          logger.warn("Unexpected error while getting DICOM series from " + dir.getAbsolutePath + " (deleting) : " + fmtEx(t))
          Utility.deleteFileTree(dir)
        }
      }
    }

    // list of files from when service was last running
    val list = ClientConfig.tmpDir.listFiles.filter(d => d.isDirectory && (!d.getName.equals(DicomMove.activeDirName)))
    list.map(dir => putSaved(dir))
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
    putSavedFiles
    poll
    eventListener
    update
  }
}
