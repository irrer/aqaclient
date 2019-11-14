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
      val result = ClientUtil.readDicomFile(fileList.head)
      val al = ClientUtil.readDicomFile(fileList.head).right.get // get one attribute list for creating the Series
      moveSeriesFiles(fileList, SeriesInstanceUID) // put all files in the same directory
      Some(al)
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
    val serUidList = DicomFind.find("RTPLAN", PatientID).map(fal => ClientUtil.getSerUid(fal)).flatten.filter(serUid => needToGet(serUid))
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

  private def monitorEvents = {
    logger.info("Need to write this") // TODO
  }

  def init = {
    poll
    monitorEvents
    update
  }
}
