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

    val didAcquire = DicomMove.dicomSemaphore.tryAcquire(ClientConfig.DicomTimeout_ms, java.util.concurrent.TimeUnit.MILLISECONDS)
    if (!didAcquire)
      logger.error("Could not acquire DICOM semaphore.  Proceeding with C-MOVE anyway.")
    val resultList: Seq[AttributeList] =
      try {

        val list = DicomCFind.cfind(
          callingAETitle = ClientConfig.DICOMClient.aeTitle,
          calledPacs = ClientConfig.DICOMSource,
          attributeList = queryAttributes,
          queryLevel = queryLevel,
          limit = None,
          queryRetrieveInformationModel = DicomCFind.QueryRetrieveInformationModel.StudyRoot
        )

        logger.info("Successfully performed DICOM C-FIND.  Number of items: " + list.size)
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
    * Get the list of SOPInstanceUID's for the given series.
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
}
