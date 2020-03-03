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

  val GracePeriod_sec = getMainText("GracePeriod_sec").toDouble

  val DataDir = getDataDir

  val seriesDir = makeChildDir(DataDir, "DICOMSeries")

  val zipDir = makeChildDir(DataDir, "tempZip")

  val staticDirFile = getExistingDir("static", Seq(""".\""", """src\main\resources\"""))

  val certificateDir = makeChildDir(staticDirFile, "certificates")

  val DICOMClient = getPacs("DICOMClient")

  val DICOMSource = getPacs("DICOMSource")

  val PollInterval_sec = getMainText("PollInterval_sec").toInt

  val MaximumDataAge = getMainText("MaximumDataAge", "100000.0").toDouble

  val AQAURL = getMainText("AQAURL")
  val AQAUser = getMainText("AQAUser")
  val AQAPassword = getMainText("AQAPassword")

  val HTTPSPort = logMainText("HTTPSPort", "443").toInt
  val AMQPBrokerHost = logMainText("AMQPBrokerHost", "localhost")
  val AMQPBrokerPort = logMainText("AMQPBrokerPort", "5672").toInt

  val DICOMRetryCount = logMainText("DICOMRetryCount", "3").toInt
  val DICOMRetryWait_sec = logMainText("DICOMRetryWait_sec", "1.0").toDouble

  /** If this is defined, then the configuration was successfully initialized. */
  val validated = true

  def validate = validated

  logger.info("Configuration has been validated.")
  logger.info(toString)
}

