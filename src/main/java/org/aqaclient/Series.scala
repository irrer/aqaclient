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
import com.pixelmed.dicom.TagFromName
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.util.Utility
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.PrettyXML

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
    ReferencedRtplanUID: Option[String]
) extends Logging {

  def this(node: Node) =
    this(
      new File(ClientConfig.seriesDir, (node \ "dir").head.text.trim),
      (node \ "SeriesInstanceUID").head.text.trim,
      (node \ "@PatientID").head.text.trim,
      Series.getDataDate(node),
      ModalityEnum.toModalityEnum((node \ "@Modality").head.text.trim),
      Series.optText(node, "FrameOfReferenceUID"),
      Series.optText(node, "RegFrameOfReferenceUID"),
      Series.optText(node, "ReferencedRtplanUID")
    )

  // @formatter:off
  def toXml: Elem = {
    <Series Modality={Modality.toString} PatientID={PatientID} dataDate={Series.xmlDateFormat.format(dataDate)}>
      <dir>{dir.getAbsolutePath.drop(ClientConfig.seriesDir.getAbsolutePath.length)}</dir>
      <SeriesInstanceUID>{SeriesInstanceUID}</SeriesInstanceUID>
      {if (FrameOfReferenceUID.isDefined) {<FrameOfReferenceUID>{FrameOfReferenceUID.get}</FrameOfReferenceUID>}}
      {if (RegFrameOfReferenceUID.isDefined) {<RegFrameOfReferenceUID>{RegFrameOfReferenceUID.get}</RegFrameOfReferenceUID>}}
      {if (ReferencedRtplanUID.isDefined) {<ReferencedRtplanUID>{ReferencedRtplanUID.get}</ReferencedRtplanUID>}}
    </Series>
  }

  // @formatter:on

  def isModality(modality: ModalityEnum.Value): Boolean = modality.toString.equalsIgnoreCase(Modality.toString)

  def isRtplan: Boolean = Modality.toString.equals(ModalityEnum.RTPLAN.toString)

  def isRecent: Boolean = {
    val cutoff = System.currentTimeMillis - ClientConfig.MaximumDataAge_ms
    dataDate.getTime > cutoff
  }

  /**
    * True if we are interested in it.  The criteria is that either it is an RTPLAN or it was created recently.
    */
  def isViable: Boolean = isRtplan || isRecent

  def ensureFilesExist(): Unit = {
    if (!(dir.isDirectory && ClientUtil.listFiles(dir).nonEmpty)) {
      DicomMove.get(SeriesInstanceUID, PatientID, Modality.toString)
    }
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
  def getString(al: AttributeList, tag: AttributeTag) =
    new String(al.get(tag).getStringValues.head)

  def dirOf(alList: Seq[AttributeList]): File = {

    /** Used for creating directory name. */
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH-mm-ss")

    val maxDate = alList.map(al => ClientUtil.dataDateTime(al)).maxBy(_.getTime)

    val patientDirName = FileUtil.replaceInvalidFileNameCharacters(
      alList.head.get(TagFromName.PatientID).getSingleStringValueOrNull,
      '_'
    )
    val patientDir = new File(ClientConfig.seriesDir, patientDirName)
    patientDir.mkdirs
    val dateText = dateFormat.format(maxDate)
    val modality = alList.head.get(TagFromName.Modality).getSingleStringValueOrDefault("unknown")
    val seriesUid = alList.head.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrDefault("unknown")
    val subDirName = FileUtil.replaceInvalidFileNameCharacters(dateText + "_" + modality + "_" + alList.size + "_" + seriesUid, '_')
    val seriesDir = new File(patientDir, subDirName)

    seriesDir
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
      dirOf(alList),
      Series.getString(al, TagFromName.SeriesInstanceUID),
      Series.getString(al, TagFromName.PatientID),
      ClientUtil.dataDateTime(al),
      ModalityEnum.toModalityEnum(Series.getString(al, TagFromName.Modality)),
      Series.getFrameOfReferenceUID(al),
      Series.getRegFrameOfReferenceUID(alList),
      Series.getReferencedRtplanUID(al)
    )

    series
  }

  private def optText(xml: Node, tag: String): Option[String] = {
    (xml \ tag).headOption match {
      case Some(node) => Some(node.text.trim)
      case _          => None
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
    val a = al.get(TagFromName.FrameOfReferenceUID)
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
      .flatMap(al => DicomUtil.findAllSingle(al, TagFromName.FrameOfReferenceUID))
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
        .get(TagFromName.ReferencedSOPInstanceUID)
        .getSingleStringValueOrEmptyString
      Some(new String(rtplanUid))
    } else
      None
  }

  /**
    * Pool of series whose DICOM contents have been fetched but have not yet been processed.
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

  def contains(SeriesInstanceUID: String): Boolean =
    get(SeriesInstanceUID).isDefined

  def getByModality(modality: ModalityEnum.Value): List[Series] =
    SeriesPool.synchronized({
      SeriesPool.values
        .filter(s => s.isModality(modality))
        .toList
        .sortBy(s => s.dataDate)
    })

  def getRtplanByFrameOfReference(
      FrameOfReferenceUID: String
  ): Option[Series] = {
    getByModality(ModalityEnum.RTPLAN).find(s =>
      s.FrameOfReferenceUID.isDefined && s.FrameOfReferenceUID.get
        .equals(FrameOfReferenceUID)
    )
  }

  def getRegByFrameOfReference(FrameOfReferenceUID: String): Option[Series] = {
    getByModality(ModalityEnum.REG).find(s =>
      s.FrameOfReferenceUID.isDefined && s.FrameOfReferenceUID.get
        .equals(FrameOfReferenceUID)
    )
  }

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
    * Update a series, including the internal pool, the XML index file, and the DICOM directory.
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
    *
    * @param maxAge_ms Maximum age in ms that files must be before they are deleted.
    */
  private def removeObsoleteZipFiles(maxAge_ms: Long = 60 * 60 * 1000): Unit = {
    def del(f: File): Unit = {
      try {
        val age = System.currentTimeMillis() - f.lastModified()
        if (age > maxAge_ms) {
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
  private def reinstateFromDicom(seriesDir: File): Option[Series] = {
    try {
      if (seriesDir.isDirectory && ClientUtil.listFiles(seriesDir).nonEmpty) {
        val series = makeSeriesFromDicomFileDir(seriesDir)
        // warn if seriesDir does not match series.dir
        logger.info("Loaded series from DICOM: " + series)
        if (!seriesDir.getAbsolutePath.equals(series.dir.getAbsolutePath)) {
          logger.warn(
            " Error in series.  Derived series.dir does not match source series dir.  The DICOM should probably be deleted.\n" +
              "    source Dir : " + seriesDir.getAbsolutePath.formatted("%-160s") + " (dir where DICOM files were found)\n" +
              "    series.dir : " + series.dir.getAbsolutePath.formatted("%-160s") + " (derived/expected/correct directory)"
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
        updatePatientXml(
          list
        ) // save them back, sorted by date with duplicates removed
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
      val patIdList = PatientProcedure.patientIdList.toSet
      val updated = list
        .groupBy(s => s.SeriesInstanceUID)
        .map(g => g._2.head)
        . // remove multiple Series that have the same series UID
        filter(s => patIdList.contains(s.PatientID))
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

  /**
    * Look at the series whose metadata is in XML or that have already been
    * fetched via C-MOVE and add a Series entry for them.
    */
  private def reinstatePreviouslyFetchedSeries(): Unit = {
    // get from XML
    ClientUtil.listFiles(ClientConfig.seriesDir).foreach(patientDir => reinstateFromXml(patientDir))

    // get DICOM files that may not be in XML
    // list of all DICOM directories
    val dirList = ClientUtil.listFiles(ClientConfig.seriesDir).flatMap(patientDir => ClientUtil.listFiles(patientDir))

    // set of all directory paths from series loaded from XML
    val dirSetFromXml = getAllSeries.map(s => s.dir.getAbsolutePath).toSet

    // list of DICOM directories that are not listed in XML
    val dirNotInXml = dirList.filterNot(dir => dirSetFromXml.contains(dir.getAbsolutePath))
    logger.info("Number of DICOM directories that were not saved in XML: " + dirNotInXml.size)

    // Make Series object from DICOM not in XML
    val seriesNotInXml = dirNotInXml.flatMap(dir => reinstateFromDicom(dir))

    seriesNotInXml.foreach(series => persist(series))

    // clean up old DICOM files
    removeOldDicom()

    // tell the uploader to check for series that needs to be uploaded
    DicomAssembleUpload.scanSeries()
  }

  /**
    * Remove series (and their files) of patients that are no longer active.
    */
  private def removeObsoletePatientSeries() = {
    val patSet = PatientProcedure.patientIdList.toSet
    getAllSeries
      .filterNot(series => patSet.contains(series.PatientID))
      .map(series => remove(series))
  }

  /**
    * Initialize series pool.
    */
  def init(): Unit = {
    logger.info("initializing Series")
    if (false) { // TODO rm keep until debugged rm
      removeObsoleteZipFiles(0)
    }
    reinstatePreviouslyFetchedSeries()
    removeObsoletePatientSeries()
    logger.info(
      "Series initialization complete.   Number of series in pool: " + Series.size
    )
  }
}
