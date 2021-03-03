package org.aqaclient

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PrettyXML
import edu.umro.ScalaUtil.Trace
import org.aqaclient.Upload.UploadSet

import java.io.File
import java.util.Date
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
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

    val imageSeriesSize: Int = {
      if (size.isDefined) size.get
      else ClientUtil.listFiles(uploadSet.imageSeries.dir).size
    }

    Trace.trace()
    // jjjjj
    def toXml: Elem = {
      Trace.trace()
      <ConfirmDicomComplete>
        <InitialUploadTime>
          {Series.xmlDateFormat.format(new Date)}
        </InitialUploadTime>{uploadSet.procedure.node}<ImageSeries size={imageSeriesSize.toString}>
        {uploadSet.imageSeries.SeriesInstanceUID}
      </ImageSeries>{
        if (uploadSet.reg.isDefined) <Reg>
        {uploadSet.reg.get.SeriesInstanceUID}
      </Reg>
      }{
        if (uploadSet.plan.isDefined) <Plan>
        {uploadSet.reg.get.SeriesInstanceUID}
      </Plan>
      }
      </ConfirmDicomComplete>
    }

    def fileName: String = {
      Trace.trace()
      val text = uploadSet.imageSeries.PatientID + "_" +
        uploadSet.imageSeries.Modality + "_" +
        Series.xmlDateFormat.format(uploadSet.imageSeries.dataDate) + "_" +
        uploadSet.procedure + "_" +
        uploadSet.imageSeries.SeriesInstanceUID + ".xml"

      Trace.trace()
      FileUtil.replaceInvalidFileNameCharacters(text.replace(' ', '_'), '_').replaceAll("___*", "_")
    }

    def file = new File(ClientConfig.confirmDicomCompleteDir, fileName)

    def persist(): Unit = {
      Trace.trace()
      val text = PrettyXML.xmlToText(toXml)
      val file = new File(ClientConfig.confirmDicomCompleteDir, fileName)
      FileUtil.writeFile(file, text)
      Trace.trace()
    }

    val timeout = new Date(ClientConfig.ConfirmDicomCompleteTimeout_ms + InitialUploadTime.getTime)
    Trace.trace()

    def msRemaining: Long = timeout.getTime - System.currentTimeMillis
    Trace.trace()

    def isActive: Boolean = msRemaining > 0

    def terminate(): Unit = {
      Trace.trace()
      file.delete
      logger.info("Completed confirmation of DICOM upload and have deleted " + file.getAbsolutePath)
    }
  }

  /**
    * Redo the given upload.
    */
  private def redoUpload(confirmState: ConfirmState): Option[UploadSet] = {
    Trace.trace()
    Series.update(confirmState.uploadSet.imageSeries.SeriesInstanceUID) match {
      case Some(series) =>
        val newUploadSet = confirmState.uploadSet.copy(imageSeries = series)
        Upload.upload(newUploadSet)
        Trace.trace()
        Some(newUploadSet)
      case _ =>
        logger.warn("Unable to update series " + confirmState.uploadSet.imageSeries)
        Trace.trace()
        None
    }
  }

  /**
    * Monitor post-upload by checking to see if any additional image slices are available from the
    * source PACS. If they are, then get them and redo the upload. If no more slices appear within
    * a configured timeout (<code>ClientConfig.ConfirmDicomCompleteInterval_sec</code>) then stop
    * monitoring it.
    *
    * When this function is called it will make at least one attempt to redo the upload. This is to
    * cover the case where the service was restarted, and even though a lot of time has elapsed, the
    * upload was not monitored during that period and so should be checked.
    */
  @tailrec
  private def monitor(confirmState: ConfirmState): Unit = {
    Trace.trace()
    def msRemaining = confirmState.timeout.getTime - System.currentTimeMillis

    logger.info(
      "Before sleep.  Monitoring DICOM upload with timeout at: " + confirmState.timeout + "    ms remaining: " + msRemaining + "/" + confirmState.msRemaining + " for " + confirmState.file.getAbsolutePath
    )

    Thread.sleep(ClientConfig.ConfirmDicomCompleteInterval_ms)
    logger.info("After sleep. Monitoring DICOM upload with timeout at: " + confirmState.timeout + "    ms remaining: " + confirmState.msRemaining + " for " + confirmState.file.getAbsolutePath)
    val newSize = DicomFind.getSliceUIDsInSeries(confirmState.uploadSet.imageSeries.SeriesInstanceUID).size

    // if the number of slices changed, then redo upload.
    if (newSize != confirmState.imageSeriesSize) {
      Trace.trace()
      logger.info("Need to redo upload.  Size was: " + confirmState.imageSeriesSize + " but changed to " + newSize + "  ms remaining: " + confirmState.msRemaining + "  for " + confirmState.fileName)

      redoUpload(confirmState) match {
        case Some(newUploadSet) =>
          val newConfirmState = ConfirmState(newUploadSet)
          newConfirmState.persist() // overwrite the previous version
          monitor(newConfirmState) // make new ConfirmState with current time
        case _ =>
          if (confirmState.isActive) {
            monitor(confirmState)
          } else {
            confirmState.terminate()
          }
      }
    } else {
      Trace.trace()
      if (confirmState.isActive) {
        monitor(confirmState)
      } else {
        confirmState.terminate()
      }
    }
  }

  /**
    * An initial upload of the given data set has been done. Put it on the list to be monitored for updates.
    */
  def confirmDicomComplete(uploadSet: UploadSet): Unit = {
    Trace.trace()
    val confirmState = ConfirmState(uploadSet)
    confirmState.persist()
    Future {
      monitor(confirmState)
    }
  }

  /**
    * Read a persisted <code>ConfirmState</code> from a file. If there are any errors then log them and
    * return None.
    */
  private def fromFile(xmlFile: File): Option[ConfirmState] = {
    try {
      Trace.trace()
      val xml = XML.loadFile(xmlFile)

      /**
        * Attempt to get a series that may or may not be specified.
        */
      def getOptSeries(tag: String): Option[Series] = {
        try {
          Series.get((xml \ tag).head.text.trim)
        } catch {
          case _: Throwable => None
        }
      }

      // jjjjj
      val InitialUploadTime = Series.xmlDateFormat.parse((xml \ "InitialUploadTime").head.text.trim)
      val procedureXml = (xml \ "Run").head
      val Procedure = new Procedure(procedureXml)
      val imageSeriesUID = (xml \ "ImageSeries").head.text.trim
      val imageSeriesSize = (xml \ "ImageSeries" \ "@size").head.text.trim.toInt
      val imageSeries = Series.get(imageSeriesUID).get

      Trace.trace()
      Trace.trace("InitialUploadTime: " + InitialUploadTime)
      Trace.trace("procedureXml: " + procedureXml)
      Trace.trace("Procedure: " + Procedure)
      Trace.trace("imageSeriesUID: " + imageSeriesUID)
      Trace.trace("imageSeriesSize: " + imageSeriesSize)
      Trace.trace("imageSeries: " + imageSeries)
      Trace.trace()

      val reg = getOptSeries("Reg")
      val plan = getOptSeries("Plan")

      val uploadSet = UploadSet(Procedure, imageSeries, reg, plan)
      val cs = ConfirmState(uploadSet, InitialUploadTime, Some(imageSeriesSize))
      Trace.trace()
      Some(cs)
    } catch {
      case t: Throwable =>
        logger.error("Unexpected error reading ConfirmDicomComplete file: " + xmlFile.getAbsolutePath + " : " + fmtEx(t))
        None
    }
  }

  /**
    * Initialize by reading any persisted in-progress <code>ConfirmState</code> and finishing them.
    */
  def init(): Unit = {
    Trace.trace()
    val confirmList = ClientUtil.listFiles(ClientConfig.confirmDicomCompleteDir).map(fromFile)
    logger.info("Number of ConfirmDicomComplete files found in " + ClientConfig.confirmDicomCompleteDir.getAbsolutePath + " : " + confirmList.size)
    Trace.trace()
    confirmList.flatten.map(c => Future { monitor(c) })
    Trace.trace()
  }

}
