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

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PrettyXML

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

  private case class ConfirmState(uploadSet: DicomAssembleUpload.UploadSetDicomCMove, InitialUploadTime: Date = new Date, size: Option[Int] = None) {

    val imageSeriesSize: Int = {
      if (size.isDefined) size.get
      else ClientUtil.listFiles(uploadSet.imageSeries.dir).size
    }

    private def regToXml: Option[Elem] = {
      try {
        if (uploadSet.reg.isDefined)
          Some(<Reg>
            {uploadSet.reg.get.SeriesInstanceUID}
          </Reg>)
        else
          None
      } catch {
        case _: Throwable => None
      }
    }

    private def planToXml: Option[Elem] = {
      try {
        if (uploadSet.plan.isDefined)
          Some(<Plan>
            {uploadSet.plan.get.SeriesInstanceUID}
          </Plan>)
        else
          None
      } catch {
        case _: Throwable => None
      }
    }

    /**
     * Format the upload as XML.  Some fields are not used programmatically, but are
     * added for debugging and diagnosing problems.
     *
     * @return Upload as XML
     */
    // @formatter:off
    private def toXml: Elem = {
      <ConfirmDicomComplete>
        <InitialUploadTime>{ Series.xmlDateFormat.format(InitialUploadTime) }</InitialUploadTime>
        { uploadSet.procedure.node }
        <ImageSeries size={ imageSeriesSize.toString }>{ uploadSet.imageSeries.SeriesInstanceUID }</ImageSeries>
        <PatientID>{uploadSet.imageSeries.PatientID}</PatientID>
        <Modality>{uploadSet.imageSeries.Modality}</Modality>
        <dataDate>{Series.xmlDateFormat.format(uploadSet.imageSeries.dataDate)}</dataDate>
        { Seq(regToXml, planToXml).flatten }
      </ConfirmDicomComplete>
    }
    // @formatter:on

    val fileName: String = {
      val text = ClientUtil.timeAsFileNameFormat.format(InitialUploadTime) + ".xml"
      text
    }

    def file = new File(ClientConfig.confirmDicomCompleteDir, fileName)

    def persist(): Unit = {
      try {
        val text = PrettyXML.xmlToText(toXml)
        val file = new File(ClientConfig.confirmDicomCompleteDir, fileName)
        FileUtil.writeFile(file, text)
      } catch {
        case t: Throwable => logger.error("Unexpected exception: " + fmtEx(t))
      }
    }

    val timeout = new Date(ClientConfig.ConfirmDicomCompleteTimeout_ms + InitialUploadTime.getTime)

    def msRemaining: Long = timeout.getTime - System.currentTimeMillis

    def isActive: Boolean = msRemaining > 0

    def terminate(): Unit = {
      file.delete
      logger.info("Completed confirmation of DICOM upload and have deleted " + file.getAbsolutePath)
    }
  }

  /**
   * Redo the given upload.
   */
  private def redoUpload(confirmState: ConfirmState): Option[DicomAssembleUpload.UploadSetDicomCMove] = {
    val series = confirmState.uploadSet.imageSeries
    Series.update(series.SeriesInstanceUID, series.PatientID, series.Modality.toString) match {
      case Some(series) =>
        val newUploadSet = {
          val us = confirmState.uploadSet
          new DicomAssembleUpload.UploadSetDicomCMove(us.procedure, us.description, series, us.reg, us.plan)
        }
        Upload.put(newUploadSet)
        Some(newUploadSet)
      case _ =>
        logger.warn("Unable to update series " + confirmState.uploadSet.imageSeries)
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
    def timeRemaining = edu.umro.ScalaUtil.Util.intervalTimeUserFriendly(confirmState.msRemaining)

    val waitTime_ms = {
      val elapsed = System.currentTimeMillis() - confirmState.InitialUploadTime.getTime
      Math.max(elapsed * 0.2, ClientConfig.ConfirmDicomCompleteInterval_ms).round
    }
    logger.info(
      "Before sleep.  Monitoring DICOM upload with timeout at: " + confirmState.timeout +
        "    time remaining: " + timeRemaining +
        " for " + confirmState.file.getAbsolutePath +
        "    wait time ms: " + edu.umro.ScalaUtil.Util.intervalTimeUserFriendly(waitTime_ms)
    )

    Thread.sleep(waitTime_ms)

    logger.info("After sleep. Monitoring DICOM upload with timeout at: " + confirmState.timeout + "    time remaining: " + timeRemaining + " for " + confirmState.file.getAbsolutePath)
    val newSize = DicomFind.getSliceUIDsInSeries(confirmState.uploadSet.imageSeries.SeriesInstanceUID).size

    // if the number of slices changed, then redo upload.
    if (newSize != confirmState.imageSeriesSize) {
      logger.info("Need to redo upload.  Size was: " + confirmState.imageSeriesSize + " but changed to " + newSize + "  time remaining: " + timeRemaining + "  for " + confirmState.fileName)

      redoUpload(confirmState) match {
        case Some(newUploadSet) =>
          val newConfirmState = ConfirmState(newUploadSet)
          newConfirmState.persist()
          confirmState.file.delete() // remove old ConfirmState file
          monitor(newConfirmState) // make new ConfirmState with current time
        case _ =>
          if (confirmState.isActive) {
            monitor(confirmState)
          } else {
            confirmState.terminate()
          }
      }
    } else {
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
  def confirmDicomComplete(uploadSet: DicomAssembleUpload.UploadSetDicomCMove): Unit = {
    val confirmState = ConfirmState(uploadSet)
    confirmState.persist()

    class Later extends Runnable {
      override def run(): Unit = {
        monitor(confirmState)
      }

      new Thread(this).start()
    }

    new Later
  }

  /**
   * Read a persisted <code>ConfirmState</code> from a file. If there are any errors then log them and
   * return None.
   */
  private def fromFile(xmlFile: File): Option[ConfirmState] = {
    try {
      logger.info("Reading confirm file " + xmlFile.getAbsolutePath)
      logger.info("Confirm file contents:\n" + FileUtil.readTextFile(xmlFile).right.get)
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

      val InitialUploadTime = Series.xmlDateFormat.parse((xml \ "InitialUploadTime").head.text.trim)
      val procedureXml = (xml \ "Procedure").head

      val Procedure = new Procedure(procedureXml)
      val imageSeriesUID = (xml \ "ImageSeries").head.text.trim
      val imageSeriesSize = (xml \ "ImageSeries" \ "@size").head.text.trim.toInt
      if (Series.get(imageSeriesUID).isEmpty) {
        logger.warn("Could not find series: " + imageSeriesUID)
        None
      } else {
        val imageSeries = Series.get(imageSeriesUID).get

        val reg = getOptSeries("Reg")
        val plan = getOptSeries("Plan")

        val uploadSet = new DicomAssembleUpload.UploadSetDicomCMove(Procedure, "redo DICOM CMove upload", imageSeries, reg, plan)
        val cs = ConfirmState(uploadSet, InitialUploadTime, Some(imageSeriesSize))
        Some(cs)
      }
    } catch {
      case t: Throwable =>
        logger.error("Unexpected error reading ConfirmDicomComplete file: " + xmlFile.getAbsolutePath + " : " + fmtEx(t))
        None
    }
  }

  /**
   * Get the list of active patient IDs in the confirm list.
   *
   * @return List of active patient IDs in the confirm list.
   */
  def getActivePatientIDList: Seq[String] = ClientUtil.listFiles(ClientConfig.confirmDicomCompleteDir).flatMap(fromFile).filter(_.isActive).map(_.uploadSet.imageSeries.PatientID)

  /**
   * Initialize by reading any persisted in-progress <code>ConfirmState</code> and finishing them.
   */
  def init(): Unit = {
    val confirmList = ClientUtil.listFiles(ClientConfig.confirmDicomCompleteDir).flatMap(fromFile)
    val path = ClientConfig.confirmDicomCompleteDir.getAbsolutePath
    logger.info(s"Number of ConfirmDicomComplete files found in $path : ${confirmList.size}    Active files: ${confirmList.count(_.isActive)}")
    confirmList.map(c => Future { monitor(c) })
  }

  def main(args: Array[String]): Unit = {
    /*
    ClientConfig.validate
    val file = new File(
      """\\hitspr\e$\Program Files\UMRO\AQAClient\data\ConfirmDicomComplete_02\_TB1_OBI_2020Q4_RTIMAGE_2021-01-15T06-53-58.000_BB_by_EPID_0.1_1.2.246.352.62.2.5229743215016869714.9802715499277461632.xml""".stripMargin
    )
    println(fromFile(file))
     */

    val min: Long = 5000
    var e: Long = 5000
    (0 to 50).foreach { i => {
      if (e > (60 * 60 * 1000)) System.exit(0)
      val w = Math.max(e * 0.2, min).round
      println(i + " : " + edu.umro.ScalaUtil.Util.intervalTimeUserFriendly(w) + " -- " + edu.umro.ScalaUtil.Util.intervalTimeUserFriendly(e))
      e = e + w
    }
    }

    println("done")
  }

}
