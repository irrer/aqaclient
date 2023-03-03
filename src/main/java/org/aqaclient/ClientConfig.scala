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

import edu.umro.ScalaUtil.PACS

import java.io.File
import scala.xml.Node

/**
  * This class extracts configuration information from the configuration file.  Refer
  * to <code>ClientConfig.xml</code> for details indicating what the different
  * configuration values are used for.
  */
object ClientConfig
    extends ClientConfigUtil(
      "AQAClientConfig.xml",
      Seq(
        ClientUtil.thisJarFile.getParentFile, // same directory as jar
        new File(System.getProperty("user.dir")), // current directory
        new File(ClientUtil.thisJarFile.getParentFile.getParentFile, """src\main\resources"""), // for development
        new File("""src\main\resources""") // for development
      )
    ) {

  val rtplanTypeList: Seq[RtplanType] = getRtplanTypeList

  /** Number of minutes into a 24 hour day at which time service should be restarted. */
  val RestartTime: Long = getHourMinuteTime("RestartTime", "3:45")

  val GracePeriod_sec: Double = logMainText("GracePeriod_sec").toDouble

  val DataDir: File = getDataDir

  val seriesDir: File = makeChildDir(DataDir, "DICOMSeries")

  val zipDir: File = makeChildDir(DataDir, "tempZip")

  val confirmDicomCompleteDir: File = makeChildDir(DataDir, "ConfirmDicomComplete")

  val staticDirFile: File = getExistingDir("static", Seq(""".\""", """src\main\resources\"""))

  val certificateDir: File = makeChildDir(staticDirFile, "certificates")

  val DICOMClient: PACS = getPacs("DICOMClient")

  val DICOMSource: PACS = getPacs("DICOMSource")

  val PollInterval_sec: Int = logMainText("PollInterval_sec").toInt

  logText("PollIntervalList", PollInterval.init((document \ "PollIntervalList").head))

  val HttpsGetTimeout_sec: Double = logMainText("HttpsGetTimeout_sec", "60.0").toDouble
  val HttpsGetTimeout_ms: Some[Long] = Some((HttpsGetTimeout_sec * 1000).round)

  val HttpsUploadTimeout_sec: Double = logMainText("HttpsUploadTimeout_sec", "30.0").toDouble
  val HttpsUploadTimeout_ms: Some[Long] = Some((HttpsUploadTimeout_sec * 1000).round)

  private def dayToMs(day: Double) = (day * 24 * 60 * 60 * 1000).round

  val MaximumDataAge_day: Double = logMainText("MaximumDataAge_day", "100000.0").toDouble
  val MaximumDataAge_ms: Long = dayToMs(MaximumDataAge_day)

  val MaximumDICOMCacheDataAge_day: Double = logMainText("MaximumDICOMCacheDataAge_day", "7.0").toDouble
  val MaximumDICOMCacheDataAge_ms: Long = dayToMs(MaximumDICOMCacheDataAge_day)

  val MaximumDICOMCacheFileAge_day: Double = logMainText("MaximumDICOMCacheFileAge_day", "3.0").toDouble
  val MaximumDICOMCacheFileAge_ms: Long = dayToMs(MaximumDICOMCacheFileAge_day)

  val AQAURL: String = logMainText("AQAURL")
  val AQAUser: String = logMainText("AQAUser")
  val AQAPassword: String = logMainText("AQAPassword")
  val ServerSocketTimeout_sec: Int = logMainText("ServerSocketTimeout_sec", "300").toInt
  val httpsClientParameters: Map[String, String] = Seq(("socketTimeout", (ServerSocketTimeout_sec * 1000).toString)).toMap

  val HTTPSPort: Int = logMainText("HTTPSPort", "443").toInt
  val AMQPBrokerHost: String = logMainText("AMQPBrokerHost", "localhost")
  val AMQPBrokerPort: Int = logMainText("AMQPBrokerPort", "5672").toInt

  val DICOMRetryCount: Int = logMainText("DICOMRetryCount", "3").toInt
  val DICOMRetryWait_sec: Double = logMainText("DICOMRetryWait_sec", "1.0").toDouble
  val DicomTimeout_sec: Double = logMainText("DicomTimeout_sec", "120.0").toDouble
  val DicomTimeout_ms: Long = (DicomTimeout_sec * 1000).round

  val ConfirmDicomCompleteInterval_sec: Double = logMainText("ConfirmDicomCompleteInterval_sec", "10.0").toDouble
  val ConfirmDicomCompleteInterval_ms: Long = (ConfirmDicomCompleteInterval_sec * 1000).round

  val ConfirmDicomCompleteTimeout_sec: Double = logMainText("ConfirmDicomCompleteTimeout_sec", "300.0").toDouble
  val ConfirmDicomCompleteTimeout_ms: Long = (ConfirmDicomCompleteTimeout_sec * 1000).round

  val ResultsRefreshInterval_min: Double = logMainText("ResultsRefreshInterval_min", "60.0").toDouble
  val ResultsRefreshInterval_ms: Long = (ResultsRefreshInterval_min * 60 * 1000).round

  val PatientProcedureAgeLimit_sec: Double = logMainText("PatientProcedureAgeLimit_sec", "60.0").toDouble
  val PatientProcedureAgeLimit_ms: Long = (PatientProcedureAgeLimit_sec * 1000).round

  val MachineLogPollingInterval_min: Double = logMainText("MachineLogPollingInterval_min", "60.0").toDouble
  val MachineLogPollingInterval_ms: Long = (MachineLogPollingInterval_min * 60 * 1000).round

  val MachineLogDirList: Seq[File] = {
    val fileList = (document \ "MachineLogDirList" \ "MachineLogDir").map(n => new File(n.text))
    fileList.filterNot(_.isDirectory).foreach(f => logger.error("Machine log dir is not a directory and is being ignored: " + f.getAbsolutePath))
    val list = fileList.filter(_.isDirectory)
    logText("MachineLogDirList \\ MachineLogDir", list.map(d => d.getAbsolutePath).mkString("\n        ", "\n        ", "\n        "))
    list
  }

  val PatientIDCFindPatternMap: Map[String, String] = {
    val list = document \ "PatientIDCFindPatternMap" \ "PatientIDCFindPattern"
    val idMap: Map[String, String] = {
      if (list.isEmpty)
        Map(): Map[String, String]
      else {
        def nodeToPair(n: Node): (String, String) = {
          val id = (n \ "@PatientID").text
          val pattern = (n \ "@pattern").text
          (id, pattern)
        }

        list.map(nodeToPair).toMap
      }
    }
    val text = idMap.keys.map(id => s"    $id :: >>${idMap(id)}<<")
    logText("PatientIDCFindPatternMap \\ PatientIDCFindPattern\n", text.mkString("\n") + "\n")
    idMap
  }

  /** If this is defined, then the configuration was successfully initialized. */
  val validated = true

  def validate: Boolean = validated

  logger.info("Configuration has been validated.")
  logger.info(toString)
}
