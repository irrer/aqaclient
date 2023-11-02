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

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PrettyXML
import edu.umro.util.Utility

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem
import scala.xml.Node
import scala.xml.XML

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
                   ReferencedRtplanUID: Option[String],
                   DeviceSerialNumber: Option[String] = None,
                   SOPInstanceUIDList: Seq[String] = Seq() // list of SOPInstanceUIDs
                 ) extends Logging {

  def this(node: Node) =
    this(
      dir = new File(ClientConfig.seriesDir, (node \ "dir").head.text.trim),
      SeriesInstanceUID = (node \ "SeriesInstanceUID").head.text.trim,
      PatientID = (node \ "@PatientID").head.text.trim,
      dataDate = Series.getDataDate(node),
      Modality = ModalityEnum.toModalityEnum((node \ "@Modality").head.text.trim),
      FrameOfReferenceUID = Series.optText(node, "FrameOfReferenceUID"),
      RegFrameOfReferenceUID = Series.optText(node, "RegFrameOfReferenceUID"),
      ReferencedRtplanUID = Series.optText(node, "ReferencedRtplanUID"),
      DeviceSerialNumber = Series.optText(node, "DeviceSerialNumber"),
      SOPInstanceUIDList = Series.getSopInstanceUidList(node)
    )

  // @formatter:off
  def toXml: Elem = {
    <Series Modality={Modality.toString} PatientID={PatientID} dataDate={Series.xmlDateFormat.format(dataDate)}>
      <dir>{dir.getAbsolutePath.drop(ClientConfig.seriesDir.getAbsolutePath.length)}</dir>
      <SeriesInstanceUID>{SeriesInstanceUID}</SeriesInstanceUID>
      {if (FrameOfReferenceUID.isDefined) {<FrameOfReferenceUID>{FrameOfReferenceUID.get}</FrameOfReferenceUID>}}
      {if (RegFrameOfReferenceUID.isDefined) {<RegFrameOfReferenceUID>{RegFrameOfReferenceUID.get}</RegFrameOfReferenceUID>}}
      {if (ReferencedRtplanUID.isDefined) {<ReferencedRtplanUID>{ReferencedRtplanUID.get}</ReferencedRtplanUID>}}
      {if (DeviceSerialNumber.isDefined) {<DeviceSerialNumber>{DeviceSerialNumber.get}</DeviceSerialNumber>}}
      <SOPInstanceUIDList>{SOPInstanceUIDList.map(s => <SOPInstanceUID>{s}</SOPInstanceUID>)}</SOPInstanceUIDList>
    </Series>
  }

    /** Earliest date of series in ms. */
    private def dataDate_ms: Long = dataDate.getTime

  // @formatter:on

  def isModality(modality: ModalityEnum.Value): Boolean = modality.toString.equalsIgnoreCase(Modality.toString)

  def isRtplan: Boolean = Modality.toString.equals(ModalityEnum.RTPLAN.toString)

  private def isRtimage: Boolean = Modality.toString.equals(ModalityEnum.RTIMAGE.toString)

  //noinspection SpellCheckingInspection
  private val isWL: Boolean = { // TODO rm when WL migration is done
    isModality(ModalityEnum.RTIMAGE) &&
      (PatientID.matches(".*QASRSWL.*") || PatientID.matches(".*TB3SRS.*"))
  }

  private def isRecent: Boolean = {
    val cutoff = System.currentTimeMillis - ClientConfig.MaximumDataAge_ms
    val is = dataDate.getTime > cutoff
    is
  }

  /**
   * True if we are interested in it.  The criteria is that either it is an RTPLAN or it was created recently.
   */
  def isViable: Boolean = {

    /**
     * Return true if, based the device serial number, the files should be submitted to the server.
     * It is not ok if the DeviceSerialNumber is defined, but is not on the valid list, which probably
     * means that it is data from an old machine.
     *
     * If the serial number is not defined, then it is not valid.  The server needs the serial number to
     * identify the machine, so submitting to the server would fail anyway.
     *
     * If the list of machines is empty, then assume that we are dealing with an old version of the server
     * that does not support getting the list of machines.
     *
     * @return True if, based on the serial number, the files should be submitted to the server.
     */
    def deviceSerialNumberIsValid: Boolean = {
      val machineDeviceSerialNumberList: Seq[String] = {
        Machine.getMachineList match {
          case Some(list) => list.flatMap(_.SerialNumber)
          case _ => Seq()
        }
      }
      DeviceSerialNumber.nonEmpty &&
        (machineDeviceSerialNumberList.isEmpty || machineDeviceSerialNumberList.contains(DeviceSerialNumber.get))
    }

    // TODO should revisit viability criteria.  Should all RTIMAGE files be required to reference an RTPLAN?  The DeviceSerialNumber should probably
    //  also be a requirement maybe for CTs too, though that would mean that non-CBCT machines (like the Philips) would fail..
    val is = 0 match {
      // Even old RTPLAN files can be useful because they are referenced by recent files.
      case _ if isRtplan =>
        true

      // Other non-RTIMAGE files should be processed if they are recent.  Note that some CBCTs do not have a DeviceSerialNumber (TX2 and TX6)
      case _ if isRecent && (!isRtimage) =>
        true

      // If an RTIMAGE file is recent and it references an RTPLAN, then it should be processed.  Sometimes
      // 'junk' RTIMAGE files are put in the system that do not reference an RTPLAN.
      case _ if isRecent && isRtimage && ReferencedRtplanUID.isDefined && deviceSerialNumberIsValid =>
        true

      // Special case for handling old Winston Lutz data.  // TODO rm this case when old WL processing is complete, though it is harmless to keep.
      case _ if ClientConfig.ProcessOldWL && isWL && isRtimage && ReferencedRtplanUID.isDefined && deviceSerialNumberIsValid =>
        true

      // Any other files are not viable.
      case _ =>
        false
    }
    is
  }

  override def toString: String = {
    "PatientID: " + PatientID + " : " + Modality + "/" + FileUtil.listFiles(dir).size + "    date: " + Series.xmlDateFormat.format(
      dataDate
    ) + "    dir: " + dir.getAbsolutePath
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
  private def getString(al: AttributeList, tag: AttributeTag) =
    new String(al.get(tag).getStringValues.head)

  private def getStringOpt(al: AttributeList, tag: AttributeTag): Option[String] = {
    try {
      Some(new String(al.get(tag).getStringValues.head))
    }
    catch {
      case _: Throwable => None
    }
  }

  /** Used for creating directory name. */
  private val dirDateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss")

  private def makePatientDirName(PatientID: String): String = FileUtil.replaceInvalidFileNameCharacters(PatientID, '_')

  /**
   * Make a series directory from parameters.  Also make the patient directory if necessary.
   *
   * @param SeriesInstanceUID SeriesInstanceUID
   * @param PatientID         PatientID
   * @param sliceCount        Number of slices in series.
   * @param Modality          Modality
   * @param date              Data date of series.
   * @return Series directory.
   */
  private def makeSeriesDir(SeriesInstanceUID: String, PatientID: String, sliceCount: Int, Modality: String, date: Date): File = {
    val patientDirName = makePatientDirName(PatientID)

    val patientDir = new File(ClientConfig.seriesDir, patientDirName)

    val dateText = dirDateFormat.format(date)

    val subDirName = {
      val text = dateText + "_" + Modality + "_" + sliceCount + "_" + SeriesInstanceUID
      FileUtil.replaceInvalidFileNameCharacters(text, '_')
    }

    val dir = new File(patientDir, subDirName)
    dir.mkdirs
    dir
  }

  /**
   * Given make a File from a Seq[AttributeList].
   *
   * @param alList Contains series content.
   * @return directory to use.
   */
  def dirOf(alList: Seq[AttributeList]): File = {
    val maxDate = alList.map(al => ClientUtil.dataDateTime(al)).maxBy(_.getTime)
    val PatientID = {
      try {
        new String(alList.head.get(TagByName.PatientID).getSingleStringValueOrNull)
      }
      catch {
        // This should NEVER happen, but we know all about that sort of situation.
        case _: Throwable => "unknown"
      }
    }
    val Modality = alList.head.get(TagByName.Modality).getSingleStringValueOrDefault("unknown")
    val SeriesInstanceUID = alList.head.get(TagByName.SeriesInstanceUID).getSingleStringValueOrDefault("unknown")

    val seriesDir = makeSeriesDir(SeriesInstanceUID, PatientID, alList.size, Modality, maxDate)
    seriesDir
  }

  /** Date to be used when a series has no slices. */
  private val dummyDate: Date = ClientUtil.timeAsFileNameFormat.parse("1800-01-01T00-00-00-000")

  /**
   * Make an empty series from the given parameters.
   *
   * @param SeriesInstanceUID SeriesInstanceUID
   * @param PatientID         PatientID
   * @param Modality          Modality
   * @return An empty series
   */
  def makeEmptySeries(SeriesInstanceUID: String, PatientID: String, Modality: String): Series = {

    val dir = makeSeriesDir(SeriesInstanceUID, PatientID, sliceCount = 0, Modality, dummyDate)

    val series = Series(
      dir = dir,
      SeriesInstanceUID = SeriesInstanceUID,
      PatientID = PatientID,
      dataDate = dummyDate,
      Modality = ModalityEnum.toModalityEnum(Modality),
      FrameOfReferenceUID = None,
      RegFrameOfReferenceUID = None,
      ReferencedRtplanUID = None,
      DeviceSerialNumber = None,
      SOPInstanceUIDList = Seq()
    )
    series
  }

  /**
   * Make a series from the DICOM files in the given directory.
   */
  def makeSeriesFromDicomFileDir(seriesDir: File): Series = {
    val alList = ClientUtil
      .listFiles(seriesDir)
      .map(f => ClientUtil.readDicomFile(f))
      .filter(e => e.isRight)
      .map(e => e.right.get)

    val al = alList.head

    val series = new Series(
      dir = dirOf(alList),
      SeriesInstanceUID = Series.getString(al, TagByName.SeriesInstanceUID),
      PatientID = Series.getString(al, TagByName.PatientID),
      dataDate = ClientUtil.dataDateTime(al),
      Modality = ModalityEnum.toModalityEnum(Series.getString(al, TagByName.Modality)),
      FrameOfReferenceUID = Series.getFrameOfReferenceUID(al),
      RegFrameOfReferenceUID = Series.getRegFrameOfReferenceUID(alList),
      ReferencedRtplanUID = Series.getReferencedRtplanUID(al),
      DeviceSerialNumber = Series.getStringOpt(al, TagByName.DeviceSerialNumber),
      SOPInstanceUIDList = alList.map(_.get(TagByName.SOPInstanceUID).getSingleStringValueOrEmptyString())
    )

    series
  }

  private def optText(xml: Node, tag: String): Option[String] = {
    (xml \ tag).headOption match {
      case Some(node) => Some(node.text.trim)
      case _ => None
    }
  }

  private def getSopInstanceUidList(xml: Node): Seq[String] = {
    (xml \ "SOPInstanceUIDList" \ "SOPInstanceUID").map(_.text.trim)
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
        val text = node.head.text.trim
        if (text.equals(unknownXmlValue)) defaultDate
        else
          xmlDateFormat.parse(text)
      }
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error parsing Series dataDate: " + fmtEx(t))
        defaultDate
    }
  }

  private def getFrameOfReferenceUID(al: AttributeList): Option[String] = {
    val a = al.get(TagByName.FrameOfReferenceUID)
    if (a == null)
      None
    else
      Some(new String(a.getSingleStringValueOrEmptyString))
  }


  /**
   * Get the frames of references that match the CTs.
   */
  private def getRegFrameOfReferenceUID(
                                         alList: Seq[AttributeList]
                                       ): Option[String] = {

    // all the frames of reference
    val allFOR = alList
      .flatMap(al => DicomUtil.findAllSingle(al, TagByName.FrameOfReferenceUID))
      .map(at => at.getSingleStringValueOrEmptyString)
      .distinct
      .filterNot(uid => uid.equals(""))

    // just the frames of references that match the RTPLAN
    val FrameOfReferenceUIDList =
      alList.flatMap(al => getFrameOfReferenceUID(al)).distinct

    // just the frames of references that match the CT (or whatever image)
    val list = allFOR.diff(FrameOfReferenceUIDList)

    if (list.nonEmpty) Some(list.mkString(" ", " ", " "))
    else None
  }

  /**
   * Get the reference RTPLAN UID if it there is one.
   */
  private def getReferencedRtplanUID(al: AttributeList): Option[String] = {
    if (al.get(TagByName.ReferencedRTPlanSequence) != null) {
      val rtplanSeq =
        DicomUtil.seqToAttr(al, TagByName.ReferencedRTPlanSequence)
      val rtplanUid = rtplanSeq.head
        .get(TagByName.ReferencedSOPInstanceUID)
        .getSingleStringValueOrEmptyString
      Some(new String(rtplanUid))
    } else
      None
  }

  /**
   * Pool of series whose DICOM contents have been fetched but have not yet been processed.
   * Key: SeriesInstanceUID
   * Value: Series
   */
  private val SeriesPool = scala.collection.mutable.HashMap[String, Series]()

  def get(SeriesInstanceUID: String): Option[Series] =
    SeriesPool.synchronized({
      SeriesPool.get(SeriesInstanceUID)
    })

  def getAllSeries: List[Series] =
    SeriesPool.synchronized {
      SeriesPool.values.toList
    }

  /**
   * Get the number of series in pool.
   */
  def size: Int =
    SeriesPool.synchronized {
      SeriesPool.size
    }

  /**
   * Return true if the given series instance UID is in the series pool.
   *
   * @param SeriesInstanceUID Look for this one.
   * @return True if it is in the pool.
   */
  def contains(SeriesInstanceUID: String): Boolean =
    get(SeriesInstanceUID).isDefined

  /**
   * Return true if there is at least one series with the given patient ID.
   *
   * @param PatientID Look for this patient ID.
   * @return True if there is at least one series for that patient.
   */
  private def containsPatientID(PatientID: String): Boolean =
    SeriesPool.synchronized {
      SeriesPool.values.exists(series => series.PatientID.equals(PatientID))
    }

  def getByModality(modality: ModalityEnum.Value): List[Series] =
    SeriesPool.synchronized({
      SeriesPool.values
        .filter(s => s.isModality(modality))
        .toList
        .sortBy(s => s.dataDate_ms)
    })

  /**
   * Find an RTPLAN that matches the given frame of reference and was created before the given date.
   *
   * @param FrameOfReferenceUID Match this.
   * @param beforeTime          Time of image series.
   * @return Qualifying plan, if found.
   */
  def getRtplanByFrameOfReference(FrameOfReferenceUID: String, beforeTime: Date): Option[Series] = {
    // limit the list to RTPLANS that match.
    val list = getByModality(ModalityEnum.RTPLAN).filter(rtplan =>
      rtplan.FrameOfReferenceUID.isDefined &&
        rtplan.FrameOfReferenceUID.get.equals(FrameOfReferenceUID) &&
        (rtplan.dataDate.getTime < beforeTime.getTime)
    )
    // If more than one match, then use the most recently created.
    list.sortBy(_.dataDate.getTime).lastOption
  }

  /*
  def getRegByFrameOfReference(FrameOfReferenceUID: String): Option[Series] = {
    getByModality(ModalityEnum.REG).find(s =>
      s.FrameOfReferenceUID.isDefined && s.FrameOfReferenceUID.get
        .equals(FrameOfReferenceUID)
    )
  }
  */

  /**
   * Get the list of REG files that have the same frame of reference as the given image file.
   *
   * @param FrameOfReferenceUID DICOM frame of reference UID to find
   * @return Series that have that frame of reference.
   */
  def getRegByRegFrameOfReference(FrameOfReferenceUID: String): Seq[Series] = {
    def forMatches(fr: String) = {
      fr.equals(FrameOfReferenceUID) ||
        fr.contains(" " + FrameOfReferenceUID + " ") ||
        fr.matches(".* " + FrameOfReferenceUID + " .*") ||
        fr.matches(FrameOfReferenceUID + " .*") ||
        fr.matches(".* " + FrameOfReferenceUID)
    }

    getByModality(ModalityEnum.REG).filter(s => s.RegFrameOfReferenceUID.isDefined && forMatches(s.RegFrameOfReferenceUID.get))
  }

  /**
   * Put a series into the pool for uploading.
   */
  private def put(series: Series, showInfo: Boolean = true) = {
    if (showInfo) logger.info("put series: " + series)
    SeriesPool.synchronized(SeriesPool.put(series.SeriesInstanceUID, series))
  }

  /**
   * Put a series into the pool for uploading.  Also notify the uploader to update.
   */
  private def putList(seriesList: Seq[Series]) = {
    SeriesPool.synchronized(
      seriesList.map(series => SeriesPool.put(series.SeriesInstanceUID, series))
    )
  }

  /**
   * Put the series in the pool and persist it's metadata in the xml file.
   */
  def persist(series: Series): Unit = {
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
  private def remove(series: Series): Unit =
    SeriesPool.synchronized {
      if (SeriesPool.contains(series.SeriesInstanceUID)) {
        logger.info("Removing local copy of series " + series)
        SeriesPool -= series.SeriesInstanceUID

      }
      try {
        if (series.dir.exists)
          Utility.deleteFileTree(series.dir)
      } catch {
        case t: Throwable =>
          logger.warn("Unexpected exception while removing Series: " + fmtEx(t))
      }
    }

  /**
   * Update a series, including the internal pool, the XML index file, and the
   * DICOM directory.  Re-get all the DICOM files.
   *
   * @param SeriesInstanceUID Series Instance UID
   * @param PatientID         Patient ID
   * @param Modality          Series Modality
   */
  def update(SeriesInstanceUID: String, PatientID: String, Modality: String): Option[Series] = {
    get(SeriesInstanceUID) match {
      case Some(oldSeries) =>
        remove(oldSeries)
      case _ =>
        logger.warn("Could not find old series with SeriesInstanceUID " + SeriesInstanceUID)
    }

    DicomMove.get(SeriesInstanceUID, PatientID: String, Modality: String) match {
      case Some(newSeries) =>
        Some(newSeries)
      case _ =>
        logger.warn(
          "Could not C-MOVE DICOM files of series with SeriesInstanceUID " + SeriesInstanceUID
        )
        None
    }
  }

  /**
   * Remove zip files that may remain from the previous instantiation of this server.
   */
  private def removeObsoleteZipFiles(): Unit = {
    def del(f: File): Unit = {
      try {
        val age = System.currentTimeMillis() - f.lastModified()
        if (age > ClientConfig.MaximumTemporaryZipFileAge_ms) {
          f.delete
          logger.info("Deleted zip file " + f.getAbsolutePath)
        }
      } catch {
        case t: Throwable =>
          logger.warn(
            "Error removing obsolete zip file " + f.getAbsolutePath + " : " + fmtEx(
              t
            )
          )
      }
    }

    def getZipList: List[File] = ClientUtil.listFiles(ClientConfig.zipDir)

    logger.info(
      "removing " + getZipList.size + " zip files from " + ClientConfig.zipDir.getAbsolutePath
    )

    getZipList.foreach(f => del(f))

    if (getZipList.nonEmpty) {
      logger.warn(
        "Unable to delete obsolete zip files: " + getZipList
          .map(f => f.getAbsolutePath)
          .mkString("\n    ", "\n    ", "\n    ")
      )
    }
  }

  /**
   * Given a directory that contains the DICOM files of a series, reinstate the Series object.
   */
  /*
  private def reinstateFromDicom(seriesDir: File): Option[Series] = {
    try {
      if (seriesDir.isDirectory && ClientUtil.listFiles(seriesDir).nonEmpty) {
        val series = makeSeriesFromDicomFileDir(seriesDir)
        // warn if seriesDir does not match series.dir
        // logger.info("Loaded series from DICOM: " + series)  // This is a nice log message but it shows too many lines.  Ends up cluttering the log file.
        if (!seriesDir.getAbsolutePath.equals(series.dir.getAbsolutePath)) {
          logger.warn(
            " Error in series.  Derived series.dir does not match source series dir.  The DICOM should probably be deleted.\n" +
              "    source Dir : " + seriesDir.getAbsolutePath.format("%-160s") + " (dir where DICOM files were found)\n" +
              "    series.dir : " + series.dir.getAbsolutePath.format("%-160s") + " (derived/expected/correct directory)"
          )
        }
        Some(series)
      } else
        None
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error while reading previously saved series from " + seriesDir.getAbsolutePath + " : " + fmtEx(t))
        None
    }
  }
  */

  /**
   * Read the index.xml files in the given patientDir.
   *
   * @param patientDir Directory for a patient's DICOM files.
   */
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
          logger.warn(
            "Problem reinstating Series from " + xmlFile.getAbsolutePath + " : " + node + " : " + fmtEx(
              t
            )
          )
          None
      }
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
            t.take(last + endTag.length)
          }
        }
        val xmlText = "<SeriesList>\n" + mainText + "\n</SeriesList>"
        val doc = XML.loadString(xmlText)
        val list = (doc \ "Series").flatMap(node => makeSeries(node))
        putList(list)
        logger.info(
          "Reinstated " + list.size + " series from " + xmlFile.getAbsolutePath
        )
        updatePatientXml(list) // save them back, sorted by date with duplicates removed
      }
    } catch {
      case t: Throwable =>
        logger.warn(
          "Problem reinstating Series from " + xmlFile.getAbsolutePath + " : " + fmtEx(
            t
          )
        )
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
      if (list.groupBy(s => s.PatientID).size > 1)
        throw new RuntimeException(
          "Error: series from more than one patient can not be in XML file."
        )
      val updated = list
        .groupBy(s => s.SeriesInstanceUID)
        .map(g => g._2.head)
        .toSeq
        .sortBy(s => s.dataDate) // sort to make them findable by a human looking at the xml

      if (updated.nonEmpty) {
        val text = updated
          .map(s => PrettyXML.xmlToText(s.toXml))
          .mkString("\n", "\n", "\n")

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
  private def removeOldDicom(): Unit = {

    def keep(s: Series) = {
      s.isRtplan ||
        s.dataDate.getTime > (System.currentTimeMillis - ClientConfig.MaximumDICOMCacheDataAge_ms) ||
        s.dir.lastModified > (System.currentTimeMillis - ClientConfig.MaximumDICOMCacheFileAge_ms)
    }

    def removeDicom(s: Series): Unit = {
      try {
        if (s.dir.isDirectory) {
          logger.info("removing DICOM files for old series: " + s)
          Utility.deleteFileTree(s.dir)
        }
      } catch {
        case t: Throwable =>
          logger.error(
            "Problem removing old DICOM series " + s.dir.getAbsolutePath + " : " + fmtEx(
              t
            )
          )
      }
    }

    logger.info("Removing (culling) old copies of DICOM files to save disk space.")
    val removeList = getAllSeries.filterNot(s => keep(s))
    logger.info("Found " + removeList.size + " old copies of DICOM series to remove.")

    removeList.foreach(s => removeDicom(s))
    logger.info("Removed old DICOM series.")
  }


  private def reIndex(): Unit = {
    val list = getAllSeries.groupBy(_.PatientID).values.map(_.sortBy(_.dataDate_ms))
    list.foreach(updatePatientXml)
  }


  /**
   * Look at the series whose metadata is in XML or that have already been
   * fetched via C-MOVE and add a Series entry for them.
   */
  private def reinstatePreviouslyFetchedSeries(): Unit = {

    case class DirSeries(dirName: String) {
      private val parts = dirName.split("_")

      val date: Date = dirDateFormat.parse(parts.head)
      val modality: ModalityEnum.Value = ModalityEnum.toModalityEnum(parts(1))
      val sliceCount: Int = parts(2).toInt
      val seriesUid: String = parts(3).trim

      override def toString: String = s"$date  $modality  $sliceCount  $seriesUid"

      val isValid: Boolean = toString.nonEmpty
    }


    //noinspection ScalaUnusedSymbol
    case class PatientInfo(PatientID: String) {
      private val dirName = makePatientDirName(PatientID)
      val dir = new File(ClientConfig.seriesDir, dirName)
    }


    // List of patients that are active.  They must be on either the patient procedure list of the list awaiting completion.
    val activePatientList = (PatientProcedure.getPatientProcedureList.map(_.patientId) ++ ConfirmDicomComplete.getActivePatientIDList).distinct.map(PatientInfo)

    val activeNotInitializePatientList = activePatientList.filterNot(patientInfo => Series.containsPatientID(patientInfo.PatientID))

    // get from XML if they are not already in the series pool.
    activeNotInitializePatientList.foreach(patientInfo => reinstateFromXml(patientInfo.dir))
    logger.info(s"Number of active patients to reinstate to XML: ${activePatientList.size}")

    // get DICOM files that may not be in XML
    // list of all DICOM directories

    def isDicomDir(dir: File): Boolean = {
      try {
        dir.isDirectory && DirSeries(dir.getName).isValid
      }
      catch {
        case _: Throwable => false
      }
    }

    val dirList = activeNotInitializePatientList.flatMap(patInfo => ClientUtil.listFiles(patInfo.dir)).filter(isDicomDir)

    // set of all directory paths from series loaded from XML
    val dirSetFromXml = getAllSeries.map(s => s.dir.getAbsolutePath).toSet

    // list of DICOM directories that are not listed in XML
    val dirNotInXml = dirList.filterNot(dir => dirSetFromXml.contains(dir.getAbsolutePath))
    logger.info("Number of DICOM directories that were not saved in XML: " + dirNotInXml.size)

    def resolveSeries(dir: File): Unit = {
      val series = makeSeriesFromDicomFileDir(dir)
      val indexedOpt = getAllSeries.find(s => s.SeriesInstanceUID.equals(series.SeriesInstanceUID))

      if (indexedOpt.nonEmpty) {
        val indexed = indexedOpt.get
        val sameDir = indexed.dir.getAbsolutePath.equals(series.dir.getAbsolutePath)
        0 match {
          case _ if sameDir => remove(indexed)
          case _ => FileUtil.deleteFileTree(indexed.dir)
        }
      }
      put(series)
    }

    // Make Series object from DICOM not in XML
    dirNotInXml.foreach(resolveSeries)

    // clean up old DICOM files
    removeOldDicom()

    // make the XML indexes match what is in memory
    reIndex()

    // tell the uploader to check for series that needs to be uploaded
    DicomAssembleUpload.scanSeries()
  }

  /**
   * Remove series (and their files) of patients that are no longer active.
   */
  /*
  private def removeObsoletePatientSeries() = {
    val patSet = PatientProcedure.patientIdList.toSet
    getAllSeries
      .filterNot(series => patSet.contains(series.PatientID))
      .map(series => remove(series))
  }
  */

  /**
   * Initialize series pool.
   */
  def init(): Unit = {
    logger.info(s"initializing Series.  Using default series date of $dummyDate")

    removeObsoleteZipFiles()
    reinstatePreviouslyFetchedSeries()
    // removeObsoletePatientSeries()
    logger.info(
      "Series initialization complete.   Number of series in pool: " + Series.size
    )
  }
}
