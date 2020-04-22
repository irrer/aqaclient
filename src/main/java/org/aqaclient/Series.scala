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
  dataDate: Option[Date],
  Modality: ModalityEnum.Value,
  FrameOfReferenceUID: Option[String], // top level frame of reference for all modalities.  For REG, this will match the one in the RTPLAN
  RegFrameOfReferenceUID: Option[String], // for REG only, will match the one in the CT
  ReferencedRtplanUID: Option[String]) extends Logging {

  def this(al: AttributeList, dir: File) = this(
    dir,
    Series.getString(al, TagFromName.SeriesInstanceUID),
    Series.getString(al, TagFromName.PatientID),
    ClientUtil.dataDateTime(al),
    ModalityEnum.toModalityEnum(Series.getString(al, TagFromName.Modality)),
    Series.getFrameOfReferenceUID(al),
    Series.getRegFrameOfReferenceUID(al),
    Series.getReferencedRtplanUID(al))

  def this(node: Node) = this(
    new File(ClientConfig.seriesDir, (node \ "dir").head.text.toString),
    (node \ "SeriesInstanceUID").head.text.toString,
    (node \ "@PatientID").head.text.toString,
    Series.getDataDate(node),
    ModalityEnum.toModalityEnum((node \ "@Modality").head.text.toString),
    Series.optText(node, "FrameOfReferenceUID"),
    Series.optText(node, "RegFrameOfReferenceUID"),
    Series.optText(node, "ReferencedRtplanUID"))

  private def dateToText(date: Option[Date]) = if (date.isDefined) Util.standardFormat(date.get) else Series.unknownXmlValue

  def toXml = {
    <Series Modality={ Modality.toString } PatientID={ PatientID } dataDate={ dateToText(dataDate) }>
      <dir>{ dir.getAbsolutePath.drop(ClientConfig.seriesDir.getAbsolutePath.size) }</dir>
      <SeriesInstanceUID>{ SeriesInstanceUID }</SeriesInstanceUID>
      { if (FrameOfReferenceUID.isDefined) <FrameOfReferenceUID>{ FrameOfReferenceUID }</FrameOfReferenceUID> }
      { if (RegFrameOfReferenceUID.isDefined) <RegFrameOfReferenceUID>{ RegFrameOfReferenceUID }</RegFrameOfReferenceUID> }
      { if (RegFrameOfReferenceUID.isDefined) <ReferencedRtplanUID>{ ReferencedRtplanUID }</ReferencedRtplanUID> }
    </Series>
  }

  def isModality(modality: ModalityEnum.Value): Boolean = modality.toString.equalsIgnoreCase(Modality.toString)

  def isRtplan = Modality.toString.equals(ModalityEnum.RTPLAN.toString)

  def isRecent = {
    val cutoff = System.currentTimeMillis - ClientConfig.MaximumDataAge_ms
    dataDate.isDefined && dataDate.get.getTime > cutoff
  }

  /**
   *  True if we are interested in it.  The criteria is that either it is an RTPLAN or it was created recently.
   */
  def isViable = isRtplan || isRecent

  override def toString: String = {
    val dateText = if (dataDate.isDefined) dataDate.get.toString else "None"
    "PatientID: " + PatientID + " : " + Modality + "    date: " + dateText + "    dir: " + dir.getAbsolutePath
  }
}

/**
 * Maintain and provide utilities to access a pool of DICOM series that have been retrieved from
 * the PACS (or RadOnc planning system) that have not been processed.
 */

object Series extends Logging {

  /**
   * Name of file in each patient directory that contains a list of Series for that patient as XML.
   */
  private val xmlFileName = "index.xml"

  /** If a value in a series is not known, then use this text in the XML. */
  private val unknownXmlValue = "unknown"

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

    val maxDate = alList.map(al => ClientUtil.dataDateTime(al)).flatten.maxBy(_.getTime)

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

  private def optText(xml: Node, tag: String): Option[String] = {
    (xml \ tag).headOption match {
      case Some(node) => Some(node.text)
      case _ => None
    }
  }

  /**
   * Parse the dataDate from Series XML.
   */
  private def getDataDate(xml: Node): Option[Date] = {
    try {
      val node = xml \ "dataDate"
      if (node.isEmpty) None
      else {
        val text = node.head.text
        if (text.equals(unknownXmlValue)) None
        else
          Some(Util.textToDate(text))
      }
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error parsing Series dataDate: " + fmtEx(t))
        None
      }
    }
  }

  private def getFrameOfReferenceUID(al: AttributeList): Option[String] = {
    val a = al.get(TagFromName.FrameOfReferenceUID)
    if (a == null)
      None
    else
      Some(a.getSingleStringValueOrEmptyString)
  }

  private def getRegFrameOfReferenceUID(al: AttributeList): Option[String] = {

    val FrameOfReferenceUID = getFrameOfReferenceUID(al)

    if (FrameOfReferenceUID.isDefined) {
      DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID).
        map(a => a.getSingleStringValueOrEmptyString).
        distinct.
        filterNot(frmRef => frmRef.equals(FrameOfReferenceUID.get)).
        headOption
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
      Some(rtplanUid)
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
      if (t.isDefined) {
        logger.warn("Unexpected error appending file " + xmlFile.getAbsolutePath + " : " + fmtEx(t.get))
      }
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
        val al = ClientUtil.readDicomFile(ClientUtil.listFiles(seriesDir).head).right.get
        val series = new Series(al, seriesDir)
        Some(series)
      } else
        None
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error while reading previously saved series from " + seriesDir.getAbsolutePath + " : " + fmtEx(t))
        None
    }
  }

  private def reinstateFromXml(patientDir: File): Unit = {
    val xmlFile = new File(patientDir, xmlFileName)

    def makeSeries(node: Node): Option[Series] = {
      try {
        Some(new Series(node))
      } catch {
        case t: Throwable =>
          logger.warn("Problem reinstating Series from " + xmlFile.getAbsolutePath + " : " + node + " : " + fmtEx(t))
          None
      }
    }

    try {
      if (xmlFile.isFile) {
        val text = "<SeriesList>\n" + FileUtil.readTextFile(xmlFile).right.get + "\n</SeriesList>"
        val doc = XML.loadString(text)
        val list = (doc \ "Series").map(node => makeSeries(node)).flatten
        putList(list)
        SeriesPool.synchronized {
          list.map(series => SeriesPool.put(series.SeriesInstanceUID, series))
        }
        logger.info("Reinstated " + list.size + " series from " + xmlFile.getAbsolutePath)
      }
    } catch {
      case t: Throwable =>
        logger.warn("Problem reinstating Series from " + xmlFile.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  /**
   * Look at the series whose metadata is in XML or that have already been
   * fetched via C-MOVE and add a Series entry for them.
   */
  private def reinststatePreviouslyFetchedSeries = {
    // get from XML
    ClientUtil.listFiles(ClientConfig.seriesDir).map(patientDir => reinstateFromXml(patientDir))

    // get DICOM files that may not be in XML
    val dirList = ClientUtil.listFiles(ClientConfig.seriesDir).map(patientDir => ClientUtil.listFiles(patientDir)).flatten.map(d => d.getAbsolutePath)
    val dicomList = dirList.map(dirName => reinstateFromDicom(new File(dirName))).flatten

    // Find series that are stored as DICOM but are not in the xml file.
    val newDicomList = dicomList.filter(ds => get(ds.SeriesInstanceUID).isEmpty)
    if (newDicomList.nonEmpty) {
      logger.info("found " + newDicomList.size + " Series entries that were not saved in the XML list.")
      newDicomList.map(series => persist(series))
    }
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
    Trace.trace("Number of series in pool: " + Series.size)
    removeObsoleteZipFiles
    Trace.trace("Number of series in pool: " + Series.size)
    reinststatePreviouslyFetchedSeries
    Trace.trace("Number of series in pool: " + Series.size)
    if (true) { // TODO rm
      Trace.trace("begin Series     -----------------------------------------")
      Trace.trace("\n" + getAllSeries.filter(s => s.Modality.toString.equals("RTIMAGE")).map(s => "SS " + s.dataDate + " " + s.Modality + " " + s.SeriesInstanceUID).mkString("\n"))
      Trace.trace("end   Series     -----------------------------------------")
    }
    removeObsoletePatientSeries
    logger.info("Series initialization complete.   Number of series in pool: " + Series.size)
  }
}