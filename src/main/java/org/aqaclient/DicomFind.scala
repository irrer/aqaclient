package org.aqaclient

import edu.umro.ScalaUtil.DicomCFind
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
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
   * Perform a C-FIND query that gets a list of series of the given modality for the given patient.
   */
  def find(modality: String, patientID: String): Seq[AttributeList] = {

    val tagSeq = Seq(TagFromName.SeriesInstanceUID)
    val tagValueSeq = Seq(
      (TagFromName.Modality, modality),
      (TagFromName.PatientID, patientID))

    val query = new Query(tagSeq, tagValueSeq).query

    ClientConfig.DICOMClient.synchronized({
      val resultList = DicomCFind.cfind(
        callingAETitle = ClientConfig.DICOMClient.aeTitle,
        calledPacs = ClientConfig.DICOMSource,
        attributeList = query,
        queryLevel = DicomCFind.QueryRetrieveLevel.SERIES,
        limit = None,
        queryRetrieveInformationModel = DicomCFind.QueryRetrieveInformationModel.StudyRoot)

      val msg = "C-FIND query PatientID: " + patientID + "    Modality: " + modality + "    number of results: " + resultList.size
      logger.info(msg)
      resultList
    })
  }

  /**
   * Get the list of SOPInstanceUID's for the given series.
   */
  def getSliceUIDsInSeries(SeriesInstanceUID: String): Seq[String] = {

    val tagSeq = Seq(TagFromName.SOPInstanceUID)
    val tagValueSeq = Seq((TagFromName.SeriesInstanceUID, SeriesInstanceUID))

    val query = new Query(tagSeq, tagValueSeq).query

    ClientConfig.DICOMClient.synchronized({
      val resultList = DicomCFind.cfind(
        callingAETitle = ClientConfig.DICOMClient.aeTitle,
        calledPacs = ClientConfig.DICOMSource,
        attributeList = query,
        queryLevel = DicomCFind.QueryRetrieveLevel.IMAGE,
        limit = None,
        queryRetrieveInformationModel = DicomCFind.QueryRetrieveInformationModel.StudyRoot)

      val seq = resultList.map(r => r.get(TagFromName.SOPInstanceUID).getSingleStringValueOrEmptyString).distinct
      val msg = "SOPInstanceUIDSeq C-FIND SeriesInstanceUID: " + SeriesInstanceUID + "    number of distinct results: " + seq.size
      logger.info(msg)
      seq
    })
  }
}
