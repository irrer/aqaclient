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

  /** Number of minutes into a 24 hour day at which time service should be restarted. */
  val RestartTime = getHourMinuteTime("RestartTime", "3:45")

  val DataDir = getDataDir

  val doneDir = makeChildDir(DataDir, doneDirName)

  val tmpDir = makeChildDir(DataDir, tmpDirName)

  val staticDirFile = getExistingDir("static", Seq(""".\""", """src\main\resources\"""))

  val DICOMClient = getPacs("DICOMClient")

  val DICOMSource = getPacs("DICOMSource")

  val PollInterval_sec = getMainText("PollInterval_sec").toInt

  val AQAURL = getMainText("AQAURL")

  val HTTPSPort = logMainText("HTTPSPort", "443").toInt
  val AMQPBroker = getAMQPBroker

  /** If this is defined, then the configuration was successfully initialized. */
  val validated = true

  def validate = validated

  logger.info("Configuration has been validated.")
  logger.info(toString)
}

