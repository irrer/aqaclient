package org.aqaclient

import java.io.File

/**
 * This class extracts configuration information from the configuration file.  Refer
 * to <code>ClientConfig.xml</code> for details indicating what the different
 * configuration values are used for.
 */
object ClientConfig extends ClientConfigUtil(
  "AQAClientConfig.xml",
  Seq(
    ClientUtil.thisJarFile.getParentFile, // same directory as jar
    new File(System.getProperty("user.dir")), // current directory
    new File(ClientUtil.thisJarFile.getParentFile.getParentFile, """src\main\resources"""), // for development
    new File("""src\main\resources""") // for development
  )) {

  val JavaKeyStorePassword = getPassword("JavaKeyStorePassword")
  val JavaKeyStoreFileList = getJavaKeyStoreFileList

  val rtplanTypeList = getRtplanTypeList

  /** Number of minutes into a 24 hour day at which time service should be restarted. */
  val RestartTime = getHourMinuteTime("RestartTime", "3:45")

  val GracePeriod_sec = logMainText("GracePeriod_sec").toDouble

  val DataDir = getDataDir

  val seriesDir = makeChildDir(DataDir, "DICOMSeries")

  val zipDir = makeChildDir(DataDir, "tempZip")

  val confirmDicomCompleteDir = makeChildDir(DataDir, "ConfirmDicomComplete")

  val staticDirFile = getExistingDir("static", Seq(""".\""", """src\main\resources\"""))

  val certificateDir = makeChildDir(staticDirFile, "certificates")

  val DICOMClient = getPacs("DICOMClient")

  val DICOMSource = getPacs("DICOMSource")

  val PollInterval_sec = logMainText("PollInterval_sec").toInt

  val HttpsGetTimeout_sec = logMainText("HttpsGetTimeout_sec", "60.0").toDouble
  val HttpsGetTimeout_ms = Some((HttpsGetTimeout_sec * 1000).round.toLong)

  val HttpsUploadTimeout_sec = logMainText("HttpsUploadTimeout_sec", "300.0").toDouble
  val HttpsUploadTimeout_ms = Some((HttpsUploadTimeout_sec * 1000).round.toLong)

  private def dayToMs(day: Double) = (day * 24 * 60 * 60 * 1000).round.toLong

  val MaximumDataAge_day = logMainText("MaximumDataAge_day", "100000.0").toDouble
  val MaximumDataAge_ms = dayToMs(MaximumDataAge_day)

  val MaximumDICOMCacheDataAge_day = logMainText("MaximumDICOMCacheDataAge_day", "7.0").toDouble
  val MaximumDICOMCacheDataAge_ms = dayToMs(MaximumDICOMCacheDataAge_day)

  val MaximumDICOMCacheFileAge_day = logMainText("MaximumDICOMCacheFileAge_day", "3.0").toDouble
  val MaximumDICOMCacheFileAge_ms = dayToMs(MaximumDICOMCacheFileAge_day)

  val AQAURL = logMainText("AQAURL")
  val AQAUser = logMainText("AQAUser")
  val AQAPassword = logMainText("AQAPassword")
  val ServerSocketTimeout_sec = logMainText("ServerSocketTimeout_sec", "300").toInt
  val httpsClientParameters = Seq(("socketTimeout", (ServerSocketTimeout_sec * 1000).toString)).toMap

  val HTTPSPort = logMainText("HTTPSPort", "443").toInt
  val AMQPBrokerHost = logMainText("AMQPBrokerHost", "localhost")
  val AMQPBrokerPort = logMainText("AMQPBrokerPort", "5672").toInt

  val DICOMRetryCount = logMainText("DICOMRetryCount", "3").toInt
  val DICOMRetryWait_sec = logMainText("DICOMRetryWait_sec", "1.0").toDouble

  val ConfirmDicomCompleteInterval_sec = logMainText("ConfirmDicomCompleteInterval_sec", "10.0").toDouble
  val ConfirmDicomCompleteInterval_ms = (ConfirmDicomCompleteInterval_sec * 1000).round
  val ConfirmDicomCompleteTimeout_sec = logMainText("ConfirmDicomCompleteTimeout_sec", "300.0").toDouble
  val ConfirmDicomCompleteTimeout_ms = (ConfirmDicomCompleteTimeout_sec * 1000).round

  /** If this is defined, then the configuration was successfully initialized. */
  val validated = true

  def validate = validated

  logger.info("Configuration has been validated.")
  logger.info(toString)
}

