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

import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.TransferSyntax
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomCFind
import edu.umro.ScalaUtil.Logging

object DicomFind extends Logging {

  private class Query(tagSeq: Seq[AttributeTag], tagValueSeq: Seq[(AttributeTag, String)]) {
    val query: AttributeList = {
      val q = new AttributeList

      def add(tag: AttributeTag): Unit = {
        val a = AttributeFactory.newAttribute(tag)
        q.put(a)
      }

      def addWithValue(tag: AttributeTag, text: String): Unit = {
        val a = AttributeFactory.newAttribute(tag)
        a.addValue(text)
        q.put(a)
      }

      tagSeq.foreach(tag => add(tag))
      tagValueSeq.foreach(tagValue => addWithValue(tagValue._1, tagValue._2))

      q
    }
  }

  /**
    * General wrapper to handle properly acquiring access to the service and possible exceptions.
    *
    * @param queryAttributes List of attributes specifying which data to get.
    * @param queryLevel DICOM query level.
    * @return List of attributes found.  Empty list on failure.
    */
  private def genericFind(queryAttributes: AttributeList, queryLevel: DicomCFind.QueryRetrieveLevel.Value): Seq[AttributeList] = {

    val transferSyntax = {
      val attr = AttributeFactory.newAttribute(TagByName.TransferSyntaxUID)
      attr.addValue(TransferSyntax.ImplicitVRLittleEndian)
      attr
    }
    queryAttributes.put(transferSyntax)

    val didAcquire = DicomMove.dicomSemaphore.tryAcquire(ClientConfig.DicomTimeout_ms, java.util.concurrent.TimeUnit.MILLISECONDS)
    if (!didAcquire)
      logger.error("Could not acquire DICOM semaphore.  Proceeding with C-MOVE anyway.")
    val resultList: Seq[AttributeList] =
      try {

        // val start = System.currentTimeMillis()
        val list = DicomCFind.cfind(
          callingAETitle = ClientConfig.DICOMClient.aeTitle,
          calledPacs = ClientConfig.DICOMSource,
          attributeList = queryAttributes,
          queryLevel = queryLevel,
          limit = None,
          queryRetrieveInformationModel = DicomCFind.QueryRetrieveInformationModel.StudyRoot
        )
        // val elapsed = System.currentTimeMillis() - start
        // logger.info("Successfully performed DICOM C-FIND.  Number of items: " + list.size + "    Elapsed ms: " + elapsed)  // message makes too much noise
        list
      } catch {
        case t: Throwable =>
          logger.error("Unexpected exception during DICOM C-MOVE: " + fmtEx(t))
          Seq()
      } finally {
        DicomMove.dicomSemaphore.release()
      }
    resultList
  }

  /**
    * Perform a C-FIND query that gets a list of series of the given modality for the given patient.
    */
  def find(modality: String, patientID: String): Seq[AttributeList] = {

    val tagSeq = Seq(TagFromName.SeriesInstanceUID)
    val tagValueSeq = Seq(
      (TagFromName.Modality, modality),
      (TagFromName.PatientID, patientID)
    )

    val queryAttributes = new Query(tagSeq, tagValueSeq).query

    val list = genericFind(queryAttributes, DicomCFind.QueryRetrieveLevel.SERIES)
    list
  }

  /**
    * Get the list of SOPInstanceUIDs for the given series.
    *
    * @param SeriesInstanceUID For this series.
    *
    * @return List of instance (slice) UIDs
    */
  def getSliceUIDsInSeries(SeriesInstanceUID: String): Seq[String] = {

    val tagSeq = Seq(TagFromName.SOPInstanceUID)
    val tagValueSeq = Seq((TagFromName.SeriesInstanceUID, SeriesInstanceUID))

    val queryAttributes = new Query(tagSeq, tagValueSeq).query

    val resultList = genericFind(queryAttributes, DicomCFind.QueryRetrieveLevel.IMAGE)

    val seq = resultList.map(r => r.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString).distinct
    val msg = "SOPInstanceUIDSeq C-FIND SeriesInstanceUID: " + SeriesInstanceUID + "    number of distinct results: " + seq.size
    logger.info(msg)
    seq
  }

  /**
    * Allow testing of C-FIND PatientID patterns on ARIA (VMSDBD).
    *
    * Usage: When prompted, enter the text to be used as the PatientID in a C-FIND.
    *
    * If an empty string is entered then a default PatientID will be used.
    *
    * @param args Not used
    */
  def main(args: Array[String]): Unit = {
    println("Starting ...")
    ClientConfig.validate

    val defaultPatientID = "$TB5_OBI_2022Q2"
    // 123456789.123456789.123456789.

    println(s"Enter PatientID C-FIND search pattern (or nothing to use default of $defaultPatientID): ")
    val s = scala.io.StdIn.readLine

    val searchPattern = {
      if (s.nonEmpty)
        s
      else
        defaultPatientID
    }

    val tagSeq = Seq(TagFromName.SeriesInstanceUID, TagByName.Modality)
    val tagValueSeq = Seq(
      (TagFromName.PatientID, searchPattern)
    )

    val queryAttributes = new Query(tagSeq, tagValueSeq).query

    val start = System.currentTimeMillis()
    val list = genericFind(queryAttributes, DicomCFind.QueryRetrieveLevel.SERIES)
    val elapsed = System.currentTimeMillis() - start

    println("---------------------------------------------------------------------------")
    println("Patient ID search pattern: >>" + searchPattern + "<<  length in characters: " + searchPattern.length)
    val patientList = list.map(_.get(TagByName.PatientID).getSingleStringValueOrEmptyString).distinct.sorted
    println("List of patient IDs found:\n    " + patientList.map(id => s">>$id<<").mkString("\n    "))
    println("Number of series found: " + list.size)
    val modalityGroups = list.groupBy(_.get(TagByName.Modality).getSingleStringValueOrEmptyString())
    val text = modalityGroups.keys.map(modality => "    " + modality.format("%-10s") + " : " + modalityGroups(modality).size.formatted("%3d"))
    println(text.mkString("\n"))
    println("Elapsed time of C-FIND in ms: " + elapsed)
    println("---------------------------------------------------------------------------")
    System.exit(0)
  }
}
