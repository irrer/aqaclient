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

  val DataDir: File = {
    def mkDir(nameList: Seq[String]): File = {
      if (nameList.isEmpty) throw new RuntimeException("Unable to create Data directory.")
      val f = new File(nameList.head)
      try {
        f.mkdirs
      } catch {
        case t: Throwable => ;
      }
      if (f.isDirectory) f else mkDir(nameList.tail)
    }
    val nameList = (document \ "DataDirList" \ "DataDir").map(node => node.head.text)
    logger.info("Trying to establish data directory from: " + nameList.mkString("    "))
    val dir = mkDir(nameList)
    logText("DataDir", dir.getAbsolutePath)
    dir
  }

  val doneDir = makeChildDir(DataDir, doneDirName)

  val tmpDir = makeChildDir(DataDir, tmpDirName)

  val staticDirFile = getExistingDir("static", Seq(""".\""", """src\main\resources\"""))

  val DICOMClient = getPacs("DICOMClient")

  val DICOMSource = getPacs("DICOMSource")

  val AQAURL = getMainText("AQAURL")

  val HTTPSPort = logMainText("HTTPSPort", "443").toInt
  val AMQPBroker = getAMQPBroker

  /** If this is defined, then the configuration was successfully initialized. */
  val validated = true

  def validate = validated

  logger.info("Configuration has been validated.")
  logger.info(toString)
}

