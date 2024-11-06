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
import com.pixelmed.dicom.TransferSyntax
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomCFind
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.Trace


/**
 * Support C-FIND calls for this service.
 */
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

    val description = s"C-FIND QueryLevel $queryLevel\n " + DicomUtil.attributeListToString(queryAttributes).split("\n").mkString("  ||  ")

    def doInSemaphore(): Seq[AttributeList] = {

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
    }

    val resultList: Seq[AttributeList] = DicomSemaphore.processInSemaphore(doInSemaphore _, description)

    resultList
  }

  /**
    * Perform a C-FIND query that gets a list of series of the given modality for the given patient.
    */
  def find(modality: String, patientID: String): Seq[AttributeList] = {

    val tagSeq = Seq(TagByName.SeriesInstanceUID)
    val tagValueSeq = Seq(
      (TagByName.Modality, modality),
      (TagByName.PatientID, patientID)
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

    val tagSeq = Seq(TagByName.SOPInstanceUID)
    val tagValueSeq = Seq((TagByName.SeriesInstanceUID, SeriesInstanceUID))

    val queryAttributes = new Query(tagSeq, tagValueSeq).query

    val resultList = genericFind(queryAttributes, DicomCFind.QueryRetrieveLevel.IMAGE)

    val seq = resultList.map(r => r.get(TagByName.SOPInstanceUID).getSingleStringValueOrEmptyString).distinct
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

    Trace.trace("\n\n\n\n\n\n\n\n============================================")
    val imageList = getSliceUIDsInSeries("1.2.246.352.62.2.4933051009168731539.6682484753785086647")
    Trace.trace(s"image list size: ${imageList.size}")
    Trace.trace(imageList)
    Trace.trace()
    val seriesList = find("CT", "$TB5_OBI_2022Q2")
    Trace.trace(s"series list size: ${seriesList.size}")
    Trace.trace(s"first series found:\n${seriesList.head}")
    Trace.trace("============================================")
    Trace.trace("Exiting ...")
    System.exit(99)

  }
}
