package org.aqaclient

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.Trace

import java.io.File
import java.text.SimpleDateFormat
import scala.xml.Node
import scala.xml.XML

object MachineLog extends Logging {

  /** Varian's date+time format. */
  private val varianDateFormat = new SimpleDateFormat("EEEE, dd MMMM yyyy HH:mm:ss")

  /** Date+time format used in XML from server. */
  private val standardDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")

  /**
    * Represent one machine log file from the I:\ drive.
    *
    * @param file Varian generated file.
    */
  private case class MachineLogFile(file: File) {
    val text: String = FileUtil.readTextFile(file).right.get
    private val node = XML.loadString(text)
    val date: Long = {
      val d = varianDateFormat.parse((node \ "DateTimeSaved").text)
      d.getTime
    }
    val DeviceSerialNumber: String = (node \ "Environment" \ "MachineSerialNumber").text
  }

  /**
    * Get a representation of a client's machine log file.  If it is already on
    * the server, then return None.
    *
    * @param file File from I:\ drive
    * @param dsnDateSet Set of machine log entries already on the server.
    * @return A machine log entry, or None.
    */
  private def makeMachineLogFile(file: File, dsnDateSet: Set[DsnDate]): Option[MachineLogFile] = {
    try {
      val machLog = MachineLogFile(file)
      if (dsnDateSet.contains(DsnDate(machLog.DeviceSerialNumber, machLog.date)))
        None // if this is already on the server, then it is irrelevant
      else
        Some(machLog)
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected exception. Unable to read machine log file " + file.getAbsolutePath + " : " + fmtEx(t))
        None
    }
  }

  /**
    * Represent one machine log entry on the server.
    *
    * @param DeviceSerialNumber Identifies the machine.
    * @param date Date and time of entry in ms.
    */
  private case class DsnDate(DeviceSerialNumber: String, date: Long) {}

  /**
    * Convert the entry for one machine into a list of DsnDates.
    *
    * @param machine Contains the machine logs from the server for one machine.
    * @return List of DsnDates.
    */
  private def toDsnDateList(machine: Node): Seq[DsnDate] = {
    val dsn = (machine \ "@DeviceSerialNumber").text

    val list = (machine \ "DateTimeSaved").map(dts => standardDateFormat.parse(dts.text).getTime).map(d => DsnDate(dsn, d))
    list
  }

  /**
    * Get all of the DeviceSerialNumber+date values that the server has.
    *
    * @return List of what the server has.
    */
  private def readFromServer(): Set[DsnDate] = {
    val xmlText = ClientUtil.httpsGet(ClientConfig.AQAURL + "/admin/MachineLogXml")
    if (xmlText.isDefined) {
      val machineList = XML.loadString(xmlText.get) \ "MachineLogDateList"
      val list = machineList.flatMap(m => toDsnDateList(m))
      list.toSet
    } else
      Set()
  }

  /**
    * Send a list of machine logs to the server.
    *
    * @param machineLogFileList List of machine logs that are new to the server sorted by time.
    */
  private def uploadToServer(machineLogFileList: Seq[MachineLogFile]): Unit = {
    if (machineLogFileList.nonEmpty) {
      val first = machineLogFileList.head
      val dirName = first.file.getParentFile.getAbsolutePath
      logger.info("Uploading " + machineLogFileList.size + " machine log entries from dir " + dirName)
      val zipFile = ClientUtil.makeZipFile(machineLogFileList.map(_.file))
      val description = "DeviceSerialNumber: " + first.DeviceSerialNumber + "    Number of new machine log entries: " + machineLogFileList.size + "    directory: " + dirName
      val procedure = Procedure.fetchList().find(_.isMachineLog).get
      val uploadSet = new UploadSet(procedure, description, zipFile)
      Upload.upload(uploadSet)
    }
  }

  /**
    * Determine which machine log entries are not on the server and send them for the given directory.
    */
  private def updateDir(dir: File, dsnDateSet: Set[DsnDate]): Unit = {
    val machLogList = dir.listFiles.toSeq.flatMap(f => makeMachineLogFile(f, dsnDateSet)).sortBy(_.date)
    machLogList.groupBy(_.DeviceSerialNumber).foreach(g => uploadToServer(g._2))
  }

  /**
    * Determine which machine log entries are not on the server and send them.
    */
  private def updateMachineLog(): Unit = {
    // get a list of machine logs already on the server
    val dsnSet = readFromServer()

    // build a list of machine logs from the client files that have not been uploaded to the server
    ClientConfig.MachineLogDirList.foreach(dir => updateDir(dir, dsnSet))
  }

  private class Poll extends Runnable {
    override def run(): Unit = {
      while (true) {
        logger.info("Updating Machine Log files.")
        updateMachineLog()
        Thread.sleep(ClientConfig.MachineLogPollingInterval_ms)
      }
    }

    new Thread(this).start()
  }

  def init(): Unit = {
    new Poll
  }
}
