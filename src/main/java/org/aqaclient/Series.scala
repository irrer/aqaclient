package org.aqaclient

import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.Logging
import scala.xml.Elem
import edu.umro.ScalaUtil.Util
import java.util.Date
import edu.umro.ScalaUtil.DicomUtil
import scala.xml.Node
import edu.umro.util.Utility
import edu.umro.ScalaUtil.Trace
import java.text.SimpleDateFormat
import edu.umro.ScalaUtil.FileUtil
import scala.xml.XML
import edu.umro.ScalaUtil.PrettyXML

/**
 * Describe a series whose DICOM has been retrieved but has not been processed.
 */
case class Series(
  dir: File,
  SeriesInstanceUID: String,
  PatientID: String,
  dataDate: Date,
  Modality: ModalityEnum.Value,
  FrameOfReferenceUID: Option[String], // top level frame of reference for all modalities.  For REG, this will match the one in the RTPLAN
  RegFrameOfReferenceUID: Option[String], // for REG only, will match the one in the CT
  ReferencedRtplanUID: Option[String]) extends Logging {

  def this(node: Node) = this(
    new File(ClientConfig.seriesDir, (node \ "dir").head.text.toString),
    (node \ "SeriesInstanceUID").head.text.toString,
    (node \ "@PatientID").head.text.toString,
    Series.getDataDate(node),
    ModalityEnum.toModalityEnum((node \ "@Modality").head.text.toString),
    Series.optText(node, "FrameOfReferenceUID"),
    Series.optText(node, "RegFrameOfReferenceUID"),
    Series.optText(node, "ReferencedRtplanUID"))

  def toXml: Elem = {

    <Series Modality={ Modality.toString } PatientID={ PatientID } dataDate={ Series.xmlDateFormat.format(dataDate) }>
      <dir>{ dir.getAbsolutePath.drop(ClientConfig.seriesDir.getAbsolutePath.size) }</dir>
      <SeriesInstanceUID>{ SeriesInstanceUID }</SeriesInstanceUID>
      { if (FrameOfReferenceUID.isDefined) <FrameOfReferenceUID>{ FrameOfReferenceUID.get }</FrameOfReferenceUID> }
      { if (RegFrameOfReferenceUID.isDefined) <RegFrameOfReferenceUID>{ RegFrameOfReferenceUID.get }</RegFrameOfReferenceUID> }
      { if (ReferencedRtplanUID.isDefined) <ReferencedRtplanUID>{ ReferencedRtplanUID.get }</ReferencedRtplanUID> }
    </Series>
  }

  def isModality(modality: ModalityEnum.Value): Boolean = modality.toString.equalsIgnoreCase(Modality.toString)

  def isRtplan = Modality.toString.equals(ModalityEnum.RTPLAN.toString)

  def isRecent = {
    val cutoff = System.currentTimeMillis - ClientConfig.MaximumDataAge_ms
    dataDate.getTime > cutoff
  }

  /**
   *  True if we are interested in it.  The criteria is that either it is an RTPLAN or it was created recently.
   */
  def isViable = isRtplan || isRecent

  def ensureFilesExist {
    if (!((dir.isDirectory) && (ClientUtil.listFiles(dir).nonEmpty))) {
      DicomMove.get(SeriesInstanceUID, toString)
    }
  }

  override def toString: String = {
    "PatientID: " + PatientID + " : " + Modality + "/" + FileUtil.listFiles(dir).size + "    date: " + Series.xmlDateFormat.format(dataDate) + "    dir: " + dir.getAbsolutePath
  }
}

/**
 * Maintain and provide utilities to access a pool of DICOM series that have been retrieved from
 * the PACS (or RadOnc planning system) that have not been processed.
 */

object Series extends Logging {

  /**
   * Name of file in each patient directory that contains a list of Series for that patient as
   * XML.  The new and old versions are transient and only exist while a new version of the
   * file is being written.
   */
  private val xmlFileName = "index.xml"
  private val xmlFileNameNew = "indexNew.xml"
  private val xmlFileNameOld = "indexOld.xml"

  /** If a value in a series is not known, then use this text in the XML. */
  private val unknownXmlValue = "unknown"

  /** Date format used when storing as XML. */
  val xmlDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss.SSS")

  /**
   * Get the given attribute as a string.  Make a new string to break the link to the AttributeList
   */
  def getString(al: AttributeList, tag: AttributeTag) = new String(al.get(tag).getStringValues.head)

  private def optDate(node: Option[Node]) = {
    try {
      Some(Util.textToDate(node.head.text))
    } catch {
      case t: Throwable => None
    }
  }

  def dirOf(alList: Seq[AttributeList]): File = {

    /** Used for creating directory name. */
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss")

    val maxDate = alList.map(al => ClientUtil.dataDateTime(al)).maxBy(_.getTime)

    val patientDirName = FileUtil.replaceInvalidFileNameCharacters(alList.head.get(TagFromName.PatientID).getSingleStringValueOrNull, '_')
    val patientDir = new File(ClientConfig.seriesDir, patientDirName)
    patientDir.mkdirs
    val dateText = dateFormat.format(maxDate)
    val modality = alList.head.get(TagFromName.Modality).getSingleStringValueOrDefault("unknown")
    val seriesUid = alList.head.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrDefault("unknown")
    val subDirName = FileUtil.replaceInvalidFileNameCharacters((dateText + "_" + modality + "_" + alList.size + "_" + seriesUid), '_')
    val seriesDir = new File(patientDir, subDirName)

    seriesDir
  }

  /**
   * Make a series from the DICOM files in the given directory.
   */
  def makeSeriesFromDicomFileDir(seriesDir: File) = {
    val alList = ClientUtil.listFiles(seriesDir).map(f => ClientUtil.readDicomFile(f)).filter(e => e.isRight).map(e => e.right.get)

    val maxDate = alList.map(al => ClientUtil.dataDateTime(al)).maxBy(_.getTime)
    val al = alList.head
    val series = new Series(
      dirOf(alList),
      Series.getString(al, TagFromName.SeriesInstanceUID),
      Series.getString(al, TagFromName.PatientID),
      ClientUtil.dataDateTime(al),
      ModalityEnum.toModalityEnum(Series.getString(al, TagFromName.Modality)),
      Series.getFrameOfReferenceUID(al),
      Series.getRegFrameOfReferenceUID(al),
      Series.getReferencedRtplanUID(al))

    series
  }

  private def optText(xml: Node, tag: String): Option[String] = {
    (xml \ tag).headOption match {
      case Some(node) => Some(node.text)
      case _ => None
    }
  }

  /**
   * Parse the dataDate from Series XML.
   */
  private def getDataDate(xml: Node): Date = {
    lazy val defaultDate = new Date
    try {
      val node = xml \ "@dataDate"
      if (node.isEmpty) defaultDate
      else {
        val text = node.head.text
        if (text.equals(unknownXmlValue)) defaultDate
        else
          xmlDateFormat.parse(text)
      }
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error parsing Series dataDate: " + fmtEx(t))
        defaultDate
      }
    }
  }

  private def getFrameOfReferenceUID(al: AttributeList): Option[String] = {
    val a = al.get(TagFromName.FrameOfReferenceUID)
    if (a == null)
      None
    else
      Some(new String(a.getSingleStringValueOrEmptyString))
  }

  private def getRegFrameOfReferenceUID(al: AttributeList): Option[String] = {

    val FrameOfReferenceUID = getFrameOfReferenceUID(al)

    if (FrameOfReferenceUID.isDefined) {
      val frmOfRef = DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID).
        map(a => a.getSingleStringValueOrEmptyString).
        distinct.
        filterNot(frmRef => frmRef.equals(FrameOfReferenceUID.get)).
        headOption
      if (frmOfRef.isDefined) Some(new String(frmOfRef.get)) else None
    } else
      None
  }

  /**
   * Get the reference RTPLAN UID if it there is one.
   */
  private def getReferencedRtplanUID(al: AttributeList): Option[String] = {
    if (al.get(TagFromName.ReferencedRTPlanSequence) != null) {
      val rtplanSeq = DicomUtil.seqToAttr(al, TagFromName.ReferencedRTPlanSequence)
      val rtplanUid = rtplanSeq.head.get(TagFromName.ReferencedSOPInstanceUID).getSingleStringValueOrEmptyString
      Some(new String(rtplanUid))
    } else
      None
  }

  /**
   * Pool of series whose DICOM contents have been fetched but have not yet been processed.
   */
  private val SeriesPool = scala.collection.mutable.HashMap[String, Series]()

  def get(SeriesInstanceUID: String): Option[Series] = SeriesPool.synchronized({
    SeriesPool.get(SeriesInstanceUID)
  })

  def getAllSeries: List[Series] = SeriesPool.synchronized {
    SeriesPool.values.toList
  }

  /**
   * Get the number of series in pool.
   */
  def size: Int = SeriesPool.synchronized { SeriesPool.size }

  def contains(SeriesInstanceUID: String) = get(SeriesInstanceUID).isDefined

  def getByModality(modality: ModalityEnum.Value): List[Series] = SeriesPool.synchronized({
    SeriesPool.values.filter(s => s.isModality(modality)).toList.sortBy(s => s.dataDate)
  })

  def getRtplanByFrameOfReference(FrameOfReferenceUID: String): Option[Series] = {
    getByModality(ModalityEnum.RTPLAN).filter(s => s.FrameOfReferenceUID.isDefined && s.FrameOfReferenceUID.get.equals(FrameOfReferenceUID)).headOption
  }

  def getRegByFrameOfReference(FrameOfReferenceUID: String): Option[Series] = {
    getByModality(ModalityEnum.REG).filter(s => s.FrameOfReferenceUID.isDefined && s.FrameOfReferenceUID.get.equals(FrameOfReferenceUID)).headOption
  }

  def getRegByRegFrameOfReference(FrameOfReferenceUID: String): Option[Series] = {
    getByModality(ModalityEnum.REG).filter(s => s.RegFrameOfReferenceUID.isDefined && s.RegFrameOfReferenceUID.get.equals(FrameOfReferenceUID)).headOption
  }

  /**
   * Put a series into the pool for uploading.  Also notify the uploader to update.
   */
  private def put(series: Series, showInfo: Boolean = true) = {
    if (showInfo) logger.info("put series: " + series)
    SeriesPool.synchronized(SeriesPool.put(series.SeriesInstanceUID, series))
  }

  /**
   * Put a series into the pool for uploading.  Also notify the uploader to update.
   */
  private def putList(seriesList: Seq[Series]) = {
    SeriesPool.synchronized(seriesList.map(series => SeriesPool.put(series.SeriesInstanceUID, series)))
  }

  /**
   * Put the series in the pool and persist it's metadata in the xml file.
   */
  def persist(series: Series) = {
    if (get(series.SeriesInstanceUID).isEmpty) {
      put(series)
      val text = "\n" + PrettyXML.xmlToText(series.toXml) + "\n"
      val xmlFile = new File(series.dir.getParentFile, xmlFileName)
      val t = FileUtil.appendFile(xmlFile, text.getBytes)
      if (t.isDefined) logger.warn("Unexpected error appending file " + xmlFile.getAbsolutePath + " : " + fmtEx(t.get))

      logger.info("Persisted series to XML: " + series)
    }
  }

  /**
   * Remove a series.
   */
  private def remove(series: Series): Unit = SeriesPool.synchronized {
    if (SeriesPool.get(series.SeriesInstanceUID).isDefined) {
      logger.info("Removing local copy of series " + series)
      SeriesPool -= series.SeriesInstanceUID
    }
    try {
      if (series.dir.exists)
        Utility.deleteFileTree(series.dir)
    } catch {
      case t: Throwable => logger.warn("Unexpected exception while removing Series: " + fmtEx(t))
    }
  }

  /**
   * Update a series, including the internal pool, the XML index file, and the DICOM directory.
   */
  def update(SeriesInstanceUID: String): Option[Series] = {
    get(SeriesInstanceUID) match {
      case Some(oldSeries) => {
        remove(oldSeries)

        DicomMove.get(SeriesInstanceUID, oldSeries.PatientID + " : " + oldSeries.Modality + " : " + oldSeries.dataDate) match {
          case Some(newSeries) => {
            Some(newSeries)
          }
          case _ => {
            logger.warn("Could not C-MOVE DICOM files of series with SeriesInstanceUID " + SeriesInstanceUID)
            None
          }
        }
      }
      case _ => {
        logger.warn("Could not find old series with SeriesInstanceUID " + SeriesInstanceUID)
        None
      }
    }
  }

  /**
   * Remove zip files that may remain from the previous instantiation of this server.
   */
  def removeObsoleteZipFiles = {
    def del(f: File) = {
      try {
        f.delete
        logger.info("Deleted zip file " + f.getAbsolutePath)
      } catch {
        case t: Throwable => logger.warn("Error removing obsolete zip file " + f.getAbsolutePath + " : " + fmtEx(t))
      }
    }

    def getZipList = ClientUtil.listFiles(ClientConfig.zipDir).toSeq
    logger.info("removing " + getZipList.size + " zip files from " + ClientConfig.zipDir.getAbsolutePath)

    getZipList.map(f => del(f))

    if (getZipList.nonEmpty) {
      logger.warn("Unable to delete obsolete zip files: " + getZipList.map(f => f.getAbsolutePath).mkString("\n    ", "\n    ", "\n    "))
    }
  }

  /**
   * Given a directory that contains the DICOM files of a series, reinstate the Series object.
   */
  private def reinstateFromDicom(seriesDir: File): Option[Series] = {
    try {
      if (seriesDir.isDirectory) {
        val series = makeSeriesFromDicomFileDir(seriesDir)
        // warn if seriesDir does not match series.dir
        logger.info("Loaded series from DICOM: " + series)
        if (!seriesDir.getAbsolutePath.equals(series.dir.getAbsolutePath)) {
          logger.warn(
            " Error in series.  Derived series.dir does not match source series dir.  The DICOM should probably be deleted.\n" +
              "    source Dir : " + seriesDir.getAbsolutePath.formatted("%-160s") + " (dir where DICOM files were found)\n" +
              "    series.dir : " + series.dir.getAbsolutePath.formatted("%-160s") + " (derived/expected/correct directory)")
          None
        } else Some(series)
      } else
        None
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error while reading previously saved series from " + seriesDir.getAbsolutePath + " : " + fmtEx(t))
        None
    }
  }

  private def reinstateFromXml(patientDir: File): Unit = {
    val xmlFile = {
      // Try different versions of the file in case there was a problem with updating it.
      val fileList = Seq(xmlFileName, xmlFileNameNew, xmlFileNameOld).map(n => new File(patientDir, n))
      val best = fileList.find(f => f.canRead)
      // handle case where this is the first time that this patient is being used and there is no xml file.
      if (best.isDefined) best.get else fileList.head
    }

    def makeSeries(node: Node): Option[Series] = {
      try {
        Some(new Series(node))
      } catch {
        case t: Throwable =>
          logger.warn("Problem reinstating Series from " + xmlFile.getAbsolutePath + " : " + node + " : " + fmtEx(t))
          None
      }
    }

    def updateFile(list: Seq[Series]): Unit = {
      val patIdList = PatientIDList.getPatientIDList
      val updated = list.
        groupBy(s => s.SeriesInstanceUID).
        map(g => g._2.head).
        filter(s => patIdList.contains(s.PatientID)).toSeq.sortBy(s => s.dataDate)
    }

    try {
      if (xmlFile.isFile) {
        val mainText = {
          val endTag = "</Series>"
          val t = FileUtil.readTextFile(xmlFile).right.get
          val last = t.lastIndexOf(endTag)
          if (last < 1)
            ""
          else {
            t.take(last + endTag.size)
          }
        }
        val xmlText = "<SeriesList>\n" + mainText + "\n</SeriesList>"
        xmlText.lastIndexOf("</Series>")
        val doc = XML.loadString(xmlText)
        val list = (doc \ "Series").map(node => makeSeries(node)).flatten
        putList(list)
        logger.info("Reinstated " + list.size + " series from " + xmlFile.getAbsolutePath)
        updatePatientXml(list) // save them back, sorted by date with duplicates removed
      }
    } catch {
      case t: Throwable =>
        logger.warn("Problem reinstating Series from " + xmlFile.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  /**
   * Update the XML file with the latest contents of the patient.  The list should
   * contain entries for only one patient.  When writing, do it using renaming so as to
   * minimize the chance of losing the contents in the event that the service is
   * unexpectedly shut down.
   */
  private def updatePatientXml(list: Seq[Series]): Unit = {
    if (list.nonEmpty) {
      if (list.groupBy(s => s.PatientID).size > 1) throw new RuntimeException("Error: series from more than one patient can not be in XML file.")
      val patIdList = PatientIDList.getPatientIDList.toSet
      val updated = list.
        groupBy(s => s.SeriesInstanceUID).map(g => g._2.head). // remove multiple Series that have the same series UID
        filter(s => patIdList.contains(s.PatientID)).toSeq.sortBy(s => s.dataDate) // sort to make them findable by a human looking at the xml

      if (updated.nonEmpty) {
        val text = updated.map(s => PrettyXML.xmlToText(s.toXml)).mkString("\n", "\n", "\n")

        val file = new File(updated.head.dir.getParentFile, xmlFileName)
        val newFile = new File(updated.head.dir.getParentFile, xmlFileNameNew)
        val oldFile = new File(updated.head.dir.getParentFile, xmlFileNameOld)

        if (!(file.canRead && FileUtil.readTextFile(file).right.get.equals(text))) { // Only over-write the file if the contents would be different.
          FileUtil.writeBinaryFile(newFile, text.getBytes)
          if (file.exists) {
            oldFile.delete
            file.renameTo(oldFile)
          }

          newFile.renameTo(file)
          oldFile.delete
        }
      }
    }
  }

  /**
   * Determine which DICOM directories should be kept based on age and remove any old ones.  An
   * index of them is still maintained in XML, but the DICOM files are removed to save disk space.
   */
  private def removeOldDicom: Unit = {

    def keep(s: Series) = {
      s.isRtplan ||
        s.dataDate.getTime > (System.currentTimeMillis - ClientConfig.MaximumDICOMCacheDataAge_ms) ||
        s.dir.lastModified > (System.currentTimeMillis - ClientConfig.MaximumDICOMCacheFileAge_ms)
    }

    def removeDicom(s: Series) = {
      try {
        if (s.dir.isDirectory) {
          logger.info("removing DICOM files for old series: " + s)
          Utility.deleteFileTree(s.dir)
        }
      } catch {
        case t: Throwable => {
          logger.error("Problem removing old DICOM series " + s.dir.getAbsolutePath + " : " + fmtEx(t))
        }
      }
    }

    logger.info("Removing (culling) old copies of DICOM files to save disk space.")

    getAllSeries.filterNot(s => keep(s)).map(s => removeDicom(s))
  }

  /**
   * Look at the series whose metadata is in XML or that have already been
   * fetched via C-MOVE and add a Series entry for them.
   */
  private def reinststatePreviouslyFetchedSeries = {
    // get from XML
    ClientUtil.listFiles(ClientConfig.seriesDir).map(patientDir => reinstateFromXml(patientDir))

    // get DICOM files that may not be in XML
    // list of all DICOM directories
    val dirList = ClientUtil.listFiles(ClientConfig.seriesDir).map(patientDir => ClientUtil.listFiles(patientDir)).flatten

    // set of all directory paths from series loaded from XML
    val dirSetFromXml = getAllSeries.map(s => s.dir.getAbsolutePath).toSet

    // list of DICOM directories that are not listed in XML
    val dirNotInXml = dirList.filterNot(dir => dirSetFromXml.contains(dir.getAbsolutePath))
    logger.info("Number of DICOM directories that were not saved in XML: " + dirNotInXml.size)

    // Make Series object from DICOM not in XML
    val seriesNotInXml = dirNotInXml.map(dir => reinstateFromDicom(dir)).flatten

    seriesNotInXml.map(series => persist(series))

    // clean up old DICOM files
    removeOldDicom

    // tell the uploader to check for series that needs to be uploaded
    Upload.scanSeries
  }

  /**
   * Remove series (and their files) of patients that are no longer active.
   */
  private def removeObsoletePatientSeries = {
    val patSet = PatientIDList.getPatientIDList.toSet
    getAllSeries.filterNot(series => patSet.contains(series.PatientID)).map(series => remove(series))
  }

  /**
   * Initialize series pool.
   */
  def init = {
    logger.info("initializing Series")
    removeObsoleteZipFiles
    reinststatePreviouslyFetchedSeries
    removeObsoletePatientSeries
    logger.info("Series initialization complete.   Number of series in pool: " + Series.size)
  }
}