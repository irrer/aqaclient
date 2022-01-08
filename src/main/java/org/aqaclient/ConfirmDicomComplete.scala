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
import edu.umro.ScalaUtil.Trace

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

    /**
      * Format the upload as XML.  Some fields are not used programmatically, but are
      * added for debugging and diagnosing problems.
      *
      * @return Upload as XML
      */
    // @formatter:off
    def toXml: Elem = {
      <ConfirmDicomComplete>
        <InitialUploadTime>{ Series.xmlDateFormat.format(InitialUploadTime) }</InitialUploadTime>
        { uploadSet.procedure.node }
        <ImageSeries size={ imageSeriesSize.toString }>{ uploadSet.imageSeries.SeriesInstanceUID }</ImageSeries>
        <PatientID>{uploadSet.imageSeries.PatientID}</PatientID>
        <Modality>{uploadSet.imageSeries.Modality}</Modality>
        <dataDate>{Series.xmlDateFormat.format(uploadSet.imageSeries.dataDate)}</dataDate>
        { if (uploadSet.reg.isDefined) <Reg>{ uploadSet.reg.get.SeriesInstanceUID }</Reg> }
        { if (uploadSet.plan.isDefined) <Plan>{ uploadSet.reg.get.SeriesInstanceUID }</Plan> }
      </ConfirmDicomComplete>
    }
    // @formatter:on

    val fileName: String = {
      val text = ClientUtil.timeAsFileNameFormat.format(InitialUploadTime) + ".xml"
      text
    }

    def file = new File(ClientConfig.confirmDicomCompleteDir, fileName)

    def persist(): Unit = {
      val text = PrettyXML.xmlToText(toXml)
      val file = new File(ClientConfig.confirmDicomCompleteDir, fileName)
      FileUtil.writeFile(file, text)
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
      Trace.trace("elapsed ms: " + elapsed)
      Math.max(elapsed, ClientConfig.ConfirmDicomCompleteInterval_ms)
    }
    logger.info(
      "Before sleep.  Monitoring DICOM upload with timeout at: " + confirmState.timeout +
        "    time remaining: " + timeRemaining +
        " for " + confirmState.file.getAbsolutePath +
        "    wait time ms: " + waitTime_ms
    )

    Trace.trace("waitTime_ms: " + waitTime_ms + "    " + fmtEx(new RuntimeException("stack trace")))
    Thread.sleep(waitTime_ms)
    Trace.trace("after sleep")

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
          Trace.trace("calling monitor")
          monitor(newConfirmState) // make new ConfirmState with current time
        case _ =>
          if (confirmState.isActive) {
            Trace.trace("calling monitor")
            monitor(confirmState)
          } else {
            Trace.trace("calling monitor")
            confirmState.terminate()
          }
      }
    } else {
      if (confirmState.isActive) {
        Trace.trace("calling monitor")
        monitor(confirmState)
      } else {
        Trace.trace("calling monitor")
        confirmState.terminate()
      }
    }
  }

  /**
    * An initial upload of the given data set has been done. Put it on the list to be monitored for updates.
    */
  def confirmDicomComplete(uploadSet: DicomAssembleUpload.UploadSetDicomCMove): Unit = {
    Trace.trace("creating confirm")
    val confirmState = ConfirmState(uploadSet)
    confirmState.persist()

    class Later extends Runnable {
      override def run(): Unit = {
        Trace.trace()
        monitor(confirmState)
        Trace.trace()
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
      if (Series.get(imageSeriesUID).isEmpty)
        logger.warn("Could not find series: " + imageSeriesUID)
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

      val uploadSet = new DicomAssembleUpload.UploadSetDicomCMove(Procedure, "redo DICOM CMove upload", imageSeries, reg, plan)
      val cs = ConfirmState(uploadSet, InitialUploadTime, Some(imageSeriesSize))
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
    val confirmList = ClientUtil.listFiles(ClientConfig.confirmDicomCompleteDir).map(fromFile)
    logger.info("Number of ConfirmDicomComplete files found in " + ClientConfig.confirmDicomCompleteDir.getAbsolutePath + " : " + confirmList.size)
    confirmList.flatten.map(c => Future { monitor(c) })
  }

  def main(args: Array[String]): Unit = {
    ClientConfig.validate
    val file = new File(
      """\\hitspr\e$\Program Files\UMRO\AQAClient\data\ConfirmDicomComplete_02\_TB1_OBI_2020Q4_RTIMAGE_2021-01-15T06-53-58.000_BB_by_EPID_0.1_1.2.246.352.62.2.5229743215016869714.9802715499277461632.xml""".stripMargin
    )
    println(fromFile(file))
    println("done")
  }

}
