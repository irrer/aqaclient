package org.aqaclient

import org.aqaclient.Upload.UploadSet
import java.util.Date
import edu.umro.ScalaUtil.FileUtil
import java.io.File
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PrettyXML
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.xml.Elem
import scala.xml.XML

/**
 * After a data set is uploaded, keep track of the DICOM series involved in case they
 * change. It is quite possible that more images were produced after the initial DICOM
 * retrieval, in which case the data set should be re-uploaded with the more complete
 * set of images.
 */
object ConfirmDicomComplete extends Logging {

  private case class ConfirmState(uploadSet: UploadSet, InitialUploadTime: Date = new Date, size: Option[Int] = None) {

    val imageSeriesSize = {
      if (size.isDefined) size.get
      else ClientUtil.listFiles(uploadSet.imageSeries.dir).size
    }

    def toXml = {
      <ConfirmDicomComplete>
        <InitialUploadTime>{ Series.xmlDateFormat.format(new Date) }</InitialUploadTime>
        { uploadSet.procedure.node }
        <ImageSeries size={ imageSeriesSize.toString }>{ uploadSet.imageSeries.SeriesInstanceUID }</ImageSeries>
        { if (uploadSet.reg.isDefined) <Reg>{ uploadSet.reg.get.SeriesInstanceUID }</Reg> }
        { if (uploadSet.plan.isDefined) <Plan>{ uploadSet.reg.get.SeriesInstanceUID }</Plan> }
      </ConfirmDicomComplete>
    }

    def fileName = {
      val text = uploadSet.imageSeries.PatientID + "_" +
        uploadSet.imageSeries.Modality + "_" +
        Series.xmlDateFormat.format(uploadSet.imageSeries.dataDate) + "_" +
        uploadSet.procedure + "_" +
        uploadSet.imageSeries.SeriesInstanceUID + ".xml"

      FileUtil.replaceInvalidFileNameCharacters(text.replace(' ', '_'), '_').replaceAll("___*", "_")
    }

    def file = new File(ClientConfig.confirmDicomCompleteDir, fileName)

    def persist = {
      val text = PrettyXML.xmlToText(toXml)
      val file = new File(ClientConfig.confirmDicomCompleteDir, fileName)
      FileUtil.writeFile(file, text)
    }
  }

  /**
   * Redo the given upload.
   */
  private def redoUpload(confirmState: ConfirmState): Unit = {
    Series.update(confirmState.uploadSet.imageSeries.SeriesInstanceUID) match {
      case Some(series) => {
        val uploadSet = new UploadSet(confirmState.uploadSet.procedure, series, confirmState.uploadSet.reg, confirmState.uploadSet.plan)
        Upload.upload(uploadSet)
      }
      case _ => logger.warn("Unable to update series " + confirmState.uploadSet.imageSeries)
    }
  }

  /**
   * Monitor post-upload by checking to see if any additional image slices are available from the
   * source PACS. If they are, then get them and redo the upload. If no more slices appear within
   * a configured timeout (<code>ClientConfig.ConfirmDicomCompleteInterval_sec</code>) then stop
   * monitoring it.
   */
  private def monitor(confirmState: ConfirmState): Unit = {
    Thread.sleep(ClientConfig.ConfirmDicomCompleteInterval_ms)
    val newSize = DicomFind.getSliceUIDsInSeries(confirmState.uploadSet.imageSeries.SeriesInstanceUID).size

    // if the number of slices changed, then redo upload.
    if (newSize != confirmState.imageSeriesSize) {
      logger.info("Need to redo upload.  Size was: " + confirmState.imageSeriesSize + " but changed to " + newSize + " for " + confirmState.fileName)
      redoUpload(confirmState)
      val newConfirmState = new ConfirmState(confirmState.uploadSet)
      newConfirmState.persist // overwrite the previous version
      monitor(newConfirmState) // make new ConfirmState with current time
    } else {
      val timeout = confirmState.InitialUploadTime.getTime + ClientConfig.ConfirmDicomCompleteInterval_ms
      if (timeout < System.currentTimeMillis) {
        logger.info("Contuing to monitor of DICOM upload for " + confirmState.file.getAbsolutePath)
        monitor(confirmState)
      } else {
        logger.info("Completed confirmation of DICOM upload and have deleted " + confirmState.file.getAbsolutePath)
        confirmState.file.delete
      }
    }
  }

  /**
   * An initial upload of the given data set has been done. Put it on the list to be monitored for updates.
   */
  def confirmDicomComplete(uploadSet: UploadSet): Unit = {
    val confirmState = new ConfirmState(uploadSet)
    confirmState.persist
    // Future { monitor(confirmState) }  // TODO put back
    monitor(confirmState) // TODO rm
  }

  /**
   * Read a persisted <code>ConfirmState</code> from a file. If there are any errors then log them and
   * return None.
   */
  private def fromFile(xmlFile: File): Option[ConfirmState] = {
    try {
      val xml = XML.loadFile(xmlFile)

      /**
       * Attempt to get a series that may or may not be specified.
       */
      def getOptSeries(tag: String): Option[Series] = {
        try {
          Series.get((xml \ tag).head.text)
        } catch {
          case t: Throwable => None
        }
      }

      val InitialUploadTime = Series.xmlDateFormat.parse((xml \ "InitialUploadTime").head.text)
      val procedureXml = (xml \ "Run").head
      val Procedure = new Procedure(procedureXml)
      val imageSeriesUID = (xml \ "ImageSeries").head.text
      val imageSeriesSize = (xml \ "ImageSeries" \ "@size").head.text.toInt
      val imageSeries = Series.get(imageSeriesUID).get

      val reg = getOptSeries("Reg")
      val plan = getOptSeries("Plan")

      val uploadSet = new UploadSet(Procedure, imageSeries, reg, plan)
      val cs = new ConfirmState(uploadSet, InitialUploadTime, Some(imageSeriesSize))
      Some(cs)
    } catch {
      case t: Throwable => {
        logger.error("Unexpected error reading ConfirmDicomComplete file: " + xmlFile.getAbsolutePath + " : " + fmtEx(t))
        None
      }
    }
  }

  /**
   * Initialize by reading any persisted in-progress <code>ConfirmState</code> and finiishing them.
   */
  def init: Unit = {
    val confirmList = ClientUtil.listFiles(ClientConfig.confirmDicomCompleteDir).map(fromFile _)
    logger.info("Number of ConfirmDicomComplete files found in " + ClientConfig.confirmDicomCompleteDir.getAbsolutePath + " : " + confirmList.size)
    // confirmList.flatten.map(c => Future { monitor(c) }) // TODO put back
    confirmList.flatten.map(monitor _) // TODO rm
  }

}