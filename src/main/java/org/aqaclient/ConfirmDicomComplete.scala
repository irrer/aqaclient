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

    /*
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
    */

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
        <ImageSeriesSliceCount>{ imageSeriesSize.toString }</ImageSeriesSliceCount>
        { uploadSet.toXml }
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
        val text = PrettyXML.xmlToText(toXml) + "\n"
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
      if (file.exists()) {
        logger.warn(s"Completed confirmation of DICOM upload but have not deleted ${file.getAbsolutePath}")
      }
      else
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
        "    wait time: " + edu.umro.ScalaUtil.Util.intervalTimeUserFriendly(waitTime_ms)
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
  private def readConfirmFromFile(xmlFile: File): Option[ConfirmState] = {
    try {
      logger.info("Reading confirm file " + xmlFile.getAbsolutePath)
      logger.info("Confirm file contents:\n" + FileUtil.readTextFile(xmlFile).right.get)
      val xml = XML.loadFile(xmlFile)

      val InitialUploadTime = Series.xmlDateFormat.parse((xml \ "InitialUploadTime").head.text.trim)
      val imageSeriesSize = (xml \ "ImageSeriesSliceCount").head.text.trim.toInt

      val uploadSet = DicomAssembleUpload.UploadSetDicomCMoveFromXml((xml \ "UploadSet").head)
      val confirmState = ConfirmState(uploadSet, InitialUploadTime, Some(imageSeriesSize))
      Some(confirmState)
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
  def getActivePatientIDList: Seq[String] = {
    ClientUtil.listFiles(ClientConfig.confirmDicomCompleteDir).flatMap(readConfirmFromFile).filter(_.isActive).map(_.uploadSet.imageSeries.PatientID)
  }

  private case class FileConfirm(xmlFile: File, confirm: Option[ConfirmState]) {}


  /**
   * Delete confirm files that could not be parsed.
   */
  private def deleteNonParsableConfirmFiles(allConfirmFiles: Seq[FileConfirm]): Unit = {
    val failedToParseList = allConfirmFiles.filter(_.confirm.isEmpty)

    def deleteFC(fc: FileConfirm): Boolean = {
      logger.warn(s"Deleting non-parsable confirm file ${fc.xmlFile.getAbsolutePath}")
      fc.xmlFile.delete()
      Thread.sleep(100)
      if (fc.xmlFile.exists()) {
        logger.warn(s"Unable to delete non-parsable confirm file ${fc.xmlFile.getAbsolutePath}")
        false
      }
      else
        true
    }

    val list = failedToParseList.map(deleteFC)

    if (list.nonEmpty) {
      logger.warn(s"Number of non-parsable confirm files found: ${list.size}.    Number of non-parsable confirm files deleted: ${list.count _}.")
    }
  }

  /**
   * Delete confirm files that have a duplicate series UID.  Keep the one that had the largest number of slices.
   */
  private def deleteRedundantConfirmFiles(listOfRedundantConfirmFiles: Seq[FileConfirm]): Unit = {

    def deleteFC(fc: FileConfirm): Boolean = {
      logger.warn(s"Deleting redundant confirm file ${fc.xmlFile.getAbsolutePath}")
      fc.xmlFile.delete()
      Thread.sleep(100)
      val result: Boolean = if (fc.xmlFile.exists()) {
        logger.warn(s"Unable to delete redundant confirm file ${fc.xmlFile.getAbsolutePath}")
        false
      }
      else {
        true
      }
      result
    }

    val deletedList = listOfRedundantConfirmFiles.map(deleteFC)
    val confirmedDeleted: Int = deletedList.filter(del => del).size
    logger.warn(s"Total number of redundant confirm files found: ${deletedList.size}.  Number of redundant confirm files deleted: $confirmedDeleted")
  }


  /**
   * Initialize by reading any persisted in-progress <code>ConfirmState</code> and finishing them.
   */
  def init(): Unit = {
    val allConfirmFiles = ClientUtil.listFiles(ClientConfig.confirmDicomCompleteDir).map(xmlFile => FileConfirm(xmlFile, readConfirmFromFile(xmlFile)))
    deleteNonParsableConfirmFiles(allConfirmFiles)

    val listOfParsableFiles = allConfirmFiles.filter(fc => fc.confirm.isDefined && fc.confirm.get.size.isDefined)

    // Group them by series, and order each group by number of slices, with the one with most slices first in the list.
    val listGroupedBySeries = listOfParsableFiles.groupBy(_.confirm.get.uploadSet.imageSeries.SeriesInstanceUID).values.map(_.sortBy(_.confirm.get.size.get).reverse)

    val listOfRedundantConfirmFiles = listGroupedBySeries.flatMap(_.tail).toSeq
    deleteRedundantConfirmFiles(listOfRedundantConfirmFiles)

    // list of active confirm files
    val listOfActiveConfirmFiles = listGroupedBySeries.map(_.head.confirm.get)

    val path = ClientConfig.confirmDicomCompleteDir.getAbsolutePath
    logger.info(s"Number of active confirm files found in $path : ${listOfActiveConfirmFiles.size}")
    listOfActiveConfirmFiles.map(c => Future {
      monitor(c)
    })
  }

  def main(args: Array[String]): Unit = {
    ClientConfig.validate
    Series.init()
    Trace.trace("\n\n\n\n\n\n\n\n========================================================================================================")
    val file = new File(
      """\\hitspr\e$\Program Files\UMRO\AQAClient\data\ConfirmDicomComplete_02\_TB1_OBI_2020Q4_RTIMAGE_2021-01-15T06-53-58.000_BB_by_EPID_0.1_1.2.246.352.62.2.5229743215016869714.9802715499277461632.xml""".stripMargin
    )
    val fc = readConfirmFromFile(new File("""D:\tmp\gapskew\baddy\2024-10-20T04-53-50-933.xml"""))

    Trace.trace(fc)

    Trace.trace("done")
  }

}
