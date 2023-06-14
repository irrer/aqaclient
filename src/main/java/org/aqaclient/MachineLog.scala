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
    * Represent one machine log entry on the server.
    *
    * @param DeviceSerialNumber Identifies the machine.
    * @param date               Date and time of entry in ms.
    */
  private case class LogEntry(file: File, DeviceSerialNumber: String, date: Long) {}

  /**
    * Represent the machine log entries from one machine that are already on the server.
    *
    * @param machineId          Common ID for machine.
    * @param DeviceSerialNumber Device serial number.
    * @param dateSet            List of dates (in ms) of machine log entries on server.
    */
  private case class ServerMachineLog(machineId: String, DeviceSerialNumber: String, dateSet: Set[Long]) {

    /**
      * Determine if the server has this entry.  Both device serial number and date must match.
      * @param dsnDate For this client entry.
      * @return True if on server, false if not.
      */
    def has(dsnDate: LogEntry): Boolean = {
      dsnDate.DeviceSerialNumber.equals(DeviceSerialNumber) && dateSet.contains(dsnDate.date)
    }
  }

  /**
    * Interpret one machine log file from the I:\ drive.
    *
    * @param file Varian generated file.
    */
  private def readMachineLogFile(file: File): LogEntry = {
    val text: String = FileUtil.readTextFile(file).right.get
    val node = XML.loadString(text)
    val date: Long = {
      val d = varianDateFormat.parse((node \ "DateTimeSaved").text)
      d.getTime
    }
    val DeviceSerialNumber: String = (node \ "Environment" \ "MachineSerialNumber").text
    LogEntry(file, DeviceSerialNumber, date)
  }

  /**
    * Validate the device serial number.  It must exist on the server.
    * @param dsn Device serial number to check.
    * @param serverList List of server machines.
    * @return True if valid.
    */
  private def isValidDeviceSerialNumber(dsn: String, serverList: Seq[ServerMachineLog]): Boolean = {
    serverList.exists(_.DeviceSerialNumber.equals(dsn))
  }

  private def serverHas(dsn: LogEntry, serverMachineLogList: Seq[ServerMachineLog]): Boolean = {
    serverMachineLogList.exists(_.has(dsn))
  }

  /**
    * Get a representation of a client's machine log file.  An entry will only be returned if both:
    *
    * The device serial number is valid (known on the server)
    *
    * The log entry is not already on the server.
    *
    * @param file File from I:\ drive
    * @param serverMachineLogList List of machine log entries already on the server.
    * @return A machine log entry, or None.
    */
  private def makeMachineLogFile(file: File, serverMachineLogList: Seq[ServerMachineLog]): Option[LogEntry] = {
    val result = {
      try {
        val logEntry = readMachineLogFile(file)

        if (isValidDeviceSerialNumber(logEntry.DeviceSerialNumber, serverMachineLogList) && (!serverHas(logEntry, serverMachineLogList)))
          Some(logEntry)
        else
          None

      } catch {
        case t: Throwable =>
          logger.warn("Unexpected exception. Unable to read machine log file " + file.getAbsolutePath + " : " + fmtEx(t))
          None
      }
    }
    if (result.isDefined)
      logger.info(s"Making machine log from file ${file.getName}")
    result
  }

  /**
    * Convert the entry for one machine into a ServerMachineLog.
    *
    * @param machine Contains the machine logs from the server for one machine.
    * @return Dates for one machine.
    */
  private def toServerMachineLog(machine: Node): ServerMachineLog = {
    val machineId = (machine \ "@machineId").text
    val DeviceSerialNumber = (machine \ "@DeviceSerialNumber").text

    val list = (machine \ "DateTimeSaved").map(dts => standardDateFormat.parse(dts.text).getTime)
    ServerMachineLog(machineId, DeviceSerialNumber, list.toSet)
  }

  /**
    * Get all of the DeviceSerialNumber+date values that the server has.
    *
    * @return List of what the server has.  If the attempt to get the list fails, then return None.
    */
  private def readFromServer(): Option[Seq[ServerMachineLog]] = {
    val xmlText = ClientUtil.httpsGet(ClientConfig.AQAURL + "/admin/MachineLogXml")
    if (xmlText.isDefined) {
      val machineList = XML.loadString(xmlText.get) \ "MachineLogDateList"
      val list = machineList.map(m => toServerMachineLog(m))
      Some(list)
    } else
      None
  }

  /**
    * Send a list of machine logs to the server.
    *
    * @param machineLogFileList List of machine logs that are new to the server sorted by time.
    */
  private def uploadToServer(machineLogFileList: Seq[LogEntry], serverMachineLogList: Seq[ServerMachineLog], procedure: Procedure): Unit = {
    if (machineLogFileList.nonEmpty) {
      val first = machineLogFileList.head
      val machineId = serverMachineLogList.find(_.DeviceSerialNumber.equals(first.DeviceSerialNumber)).get.machineId
      val dirName = first.file.getParentFile.getAbsolutePath
      val description = s"Found ${machineLogFileList.size} machine log entries from dir $dirName for machine $machineId : ${first.DeviceSerialNumber}"
      logger.info(description)
      val zipFile = ClientUtil.makeZipFile(machineLogFileList.map(_.file), description = "MachineLog_" + machineId)
      val uploadSet = new UploadSet(procedure, description, zipFile)
      Upload.put(uploadSet)
    }
  }

  /**
    * Determine which machine log entries are not on the server and send them for the given directory.
    */
  private def findNewEntries(dir: File, serverMachineLogList: Seq[ServerMachineLog]): Seq[LogEntry] = {
    if (dir.isDirectory) {
      logger.info(s"Processing machine log directory ${dir.getAbsolutePath}")
      val clientMachineLogList = FileUtil.listFiles(dir).flatMap(f => makeMachineLogFile(f, serverMachineLogList)).sortBy(_.date)
      clientMachineLogList
    } else {
      logger.warn(s"Unexpected configuration error (Exception): machine log directory does not exist: ${dir.getAbsolutePath}")
      Seq()
    }
  }

  /**
    * Send machine log entries to the server.
    * @param serverMachineLogList List of machine log entries that are already on the server.
    */
  private def updateMachineLog(serverMachineLogList: Seq[ServerMachineLog], procedure: Procedure): Unit = {
    // build a list of machine logs from the client files that have not been uploaded to the server
    val list = ClientConfig.MachineLogDirList.flatMap(dir => findNewEntries(dir, serverMachineLogList))

    val byMachine = list.groupBy(_.DeviceSerialNumber).values.filter(_.nonEmpty)
    byMachine.foreach(m => uploadToServer(m, serverMachineLogList, procedure))
  }

  private class Poll extends Runnable {

    override def run(): Unit = {

      while (true) {
        try {
          Procedure.fetchList().find(_.isMachineLog) match {
            case Some(machineLogProcedure) =>
              readFromServer() match {
                case Some(serverMachineLog) =>
                  logger.info("Starting to update Machine Log files...")
                  val start = System.currentTimeMillis()
                  updateMachineLog(serverMachineLog, machineLogProcedure)
                  val elapsed = System.currentTimeMillis() - start
                  logger.info(s"Finished updating Machine Log files.  Elapsed time in ms: $elapsed")
                case _ =>
                  logger.warn(s"Unable to retrieve machine log from server.")
              }
            case _ => logger.info("MachineLog procedure not defined on the AQA server.")
          }

        } catch {
          case t: Throwable => logger.warn(s"Unexpected error: ${fmtEx(t)}")
        }
        Thread.sleep(ClientConfig.MachineLogPollingInterval_ms)
      }
    }

    new Thread(this).start()
  }

  def init(): Unit = {
    new Poll
  }

  /**
    * For testing only.
    * This tests uploading machine log files to the AQA server.
    * @param args Not used.
    */
  def main(args: Array[String]): Unit = {
    ClientConfig.validate
    Trace.trace("Starting")

    if (false) {
      val srvList = readFromServer()
      if (srvList.isEmpty)
        Trace.trace("unable to get list of server log")
      else
        Trace.trace(s"Number of machines: ${srvList.size}   Number of log entries: ${srvList.get.flatMap(_.dateSet).size}")
    }

    Series.init()

    if (true) {
      HttpsInit.init()
      PatientProcedure.init()
      Upload.init()
      Thread.sleep(500)
      init()

      Trace.trace("Done but for sleeping.")
      Thread.sleep(1000000 * 60 * 1000)
    }

    Trace.trace("Done")
    System.exit(99)

  }
}
