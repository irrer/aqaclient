package org.aqaclient

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PrettyXML

import java.io.File
import java.util.Date
import scala.annotation.tailrec
import scala.xml.Elem

/*
 * Copyright 2024 Regents of the University of Michigan
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

/**
  * Represent an upload set that has been sent to the AQA server.  In general, one attempt should
  * be made for each data set regardless of whether is succeeds or fails.  If it succeeds, then it
  * does not need to be sent again.  If it fails, then retrying probably won't help, and this client
  * could get stuck retrying the same bad data set over and over.
  *
  * The reason that the Results is not sufficient is the case of an occasional faulty data
  * set that the AQA refuses to process.  When uploaded, the AQA returns a failure status.  The data
  * set never shows up in the Results, so the client tries it over and over to the exclusion
  * of uploading anything else.
  *
  * Sent files are kept until at least one of the following happens:
  *
  * <ul>
  *   <li>have results on the server (success) </li>
  *   <li>no longer on the list of patient being scanned (irrelevant)</li>
  *   <li>reference an old series (obsolete)</li>
  * </ul>
  *
  * A list of these is kept in memory, so a server restart will clear them out, and failed cases
  * will be re-sent.  There might also be a web interface in the future for re-sending a data set.
  *
  * @param SeriesInstanceUID Series UID.
  * @param Modality Modality of series.
  * @param PatientID patient ID
  * @param procedureName name of procedure
  * @param procedureVersion version of procedure
  * @param sentDateTime date+time when files were sent
  */
case class Sent(
    SeriesInstanceUID: String,
    SeriesDateTime: Date,
    Modality: String,
    PatientID: String,
    ReferencedRtplanUID: Option[String],
    procedureName: String,
    procedureVersion: String,
    sentDateTime: Date = new Date
) extends Logging {

  private val fileName = {
    val d = ClientUtil.timeAsFileNameFormat.format(sentDateTime)
    val text = FileUtil.replaceInvalidFileNameCharacters(s"$d $PatientID $Modality $procedureName $procedureVersion", '_')
    text.replaceAll(" ", "_").replaceAll("__+", "_") + ".xml"
  }

  private val file = {
    Sent.sentDir.mkdirs()
    new File(Sent.sentDir, fileName)
  }

  private def toXml: Elem = {
    def date2Text(date: Date): String = edu.umro.ScalaUtil.Util.formatDate(ClientUtil.standardDateFormat, date)

    def refPlanUid: Seq[Elem] = {
      if (ReferencedRtplanUID.isDefined) {
        Seq(<ReferencedRtplanUID>{ReferencedRtplanUID.get}</ReferencedRtplanUID>)
      } else
        Seq()
    }

    <Sent>
      <SeriesInstanceUID>{SeriesInstanceUID}</SeriesInstanceUID>
      <SeriesDateTime>{date2Text(SeriesDateTime)}</SeriesDateTime>
      <Modality>{Modality}</Modality>
      <PatientID>{PatientID}</PatientID>
      {refPlanUid}
      <procedureName>{procedureName}</procedureName>
      <procedureVersion>{procedureVersion}</procedureVersion>
      <sentDateTime>{date2Text(sentDateTime)}</sentDateTime>
    </Sent>
  }

  private def persist(): Unit = {
    val text = PrettyXML.xmlToText(toXml) + "\n"
    FileUtil.writeFile(file, text)
    logger.info(s"Saved Sent file ${file.getAbsolutePath}")
  }
}

object Sent extends Logging {

  private def sentDir: File = new File(ClientConfig.DataDir, "Sent")

  /**
    * Pool of series whose DICOM contents have been fetched but have not yet been processed.
    */
  private val SentList = scala.collection.mutable.ArrayBuffer[Sent]()

  /**
    * Add a record to the list.
    */
  private def addToMemory(sent: Sent): Unit = {
    SentList.synchronized {
      SentList.append(sent)
    }
  }

  /**
    * Add a record to the list.
    */
  def add(sent: Sent): Unit = {
    sent.persist()
    addToMemory(sent)
  }

  /**
    * Return true if the list contains a series with the given series UID.
    */
  def hasImageSeries(SeriesInstanceUID: String): Boolean = {
    SentList.synchronized {
      SentList.exists(_.SeriesInstanceUID.equals(SeriesInstanceUID))
    }
  }

  /**
    * Read a <code>Sent</code> object from the given file.  If anything goes wrong (parsing, file access, etc.) then return None.
    *
    * @param file From this file.
    * @return Sent object
    */
  private def read(file: File): Option[Sent] = {
    try {
      val elem = scala.xml.XML.loadFile(file)

      def get(tag: String): String = (elem \ tag).head.text

      def getOpt(tag: String): Option[String] = {
        try {
          Some(get(tag))
        } catch {
          case _: Throwable => None
        }
      }

      def text2date(text: String): Date = edu.umro.ScalaUtil.Util.parseDate(ClientUtil.standardDateFormat, text)
      def getDate(tag: String) = text2date(get(tag))

      val sent = Sent(
        SeriesInstanceUID = get("SeriesInstanceUID"),
        SeriesDateTime = getDate("SeriesDateTime"),
        Modality = get("Modality"),
        PatientID = get("PatientID"),
        ReferencedRtplanUID = getOpt("ReferencedRtplanUID"),
        procedureName = get("procedureName"),
        procedureVersion = get("procedureVersion"),
        sentDateTime = getDate("sentDateTime")
      )
      Some(sent)
    } catch {
      case t: Throwable =>
        logger.warn(s"Error reading Sent file.  Ignoring ${file.getAbsolutePath} : ${fmtEx(t)}")
        None
    }
  }

  def makeFromUploadSet(uploadSet: DicomAssembleUpload.UploadSetDicomCMove, dateTime: Date = new Date): Sent = {
    Sent(
      SeriesInstanceUID = uploadSet.imageSeries.SeriesInstanceUID,
      SeriesDateTime = uploadSet.imageSeries.seriesDateTime,
      Modality = uploadSet.imageSeries.Modality.toString,
      PatientID = uploadSet.imageSeries.PatientID,
      ReferencedRtplanUID = uploadSet.imageSeries.ReferencedRtplanUID,
      procedureName = uploadSet.procedure.Name,
      procedureVersion = uploadSet.procedure.Version,
      sentDateTime = dateTime
    )
  }

  /**
    * Remove any Sent entries that have results or are old.
    */
  def removeIrrelevantRecords(): Unit = {

    /** List of active patients. */
    val patientIdSet = PatientProcedure.patientIdList.toSet

    /**
      * Determine if a record should be removed.  To be removed, it must
      * either
      * <ul>
      *   <li>no longer be on the list of patient being scanned</li>
      *   <li>have results on the server</li>
      *   <li>reference an old series</li>
      * </ul>
      * @param sent Test this.
      * @return True if it should be removed.
      */
    def shouldBeRemoved(sent: Sent): Boolean = {
      def irrelevant = {
        Series.get(sent.SeriesInstanceUID) match {
          case Some(series) => !series.isViable
          case _            => false // do not remove it just because there is no record of it.
        }
      }
      (!patientIdSet.contains(sent.PatientID)) || Results.containsSeries(sent.PatientID, sent.SeriesInstanceUID) || irrelevant
    }

    @tailrec
    def del(): Unit = {
      val i = SentList.indexWhere(shouldBeRemoved)
      if (i != -1) {
        SentList(i).file.delete()
        SentList.synchronized { SentList.remove(i) }
        Thread.sleep(100) // let file system catch up
        del()
      }

    }
    del()
  }

  /**
    * Read the saved files.  Remove any that have results.
    */
  def init(): Unit = {
    sentDir.mkdirs()

    val list = FileUtil.listFiles(sentDir).flatMap(read)
    list.foreach(addToMemory)
    logger.info(s"Read ${SentList.size} Sent items from files.")
    removeIrrelevantRecords()
    logger.info(s"Finished Sent initialization. ${SentList.size} Sent records remain.")
  }

  def main(args: Array[String]): Unit = {
    ClientConfig.validate

    sentDir.mkdirs()
    val sent = Sent(
      SeriesInstanceUID = "SerUID",
      SeriesDateTime = new Date(System.currentTimeMillis() - 24 * 60 * 60 * 1000),
      Modality = "Moldy",
      PatientID = "ThePat",
      // ReferencedRtplanUID = Some("RefPlan"),
      ReferencedRtplanUID = None,
      procedureName = "ProcName",
      procedureVersion = "ProcVer",
      sentDateTime = new Date
    )
    sent.persist()
    println("sent before: " + sent)
    val sent2 = read(sent.file)
    println("sent after : " + sent2.get)
  }

}
