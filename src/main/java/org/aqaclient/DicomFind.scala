package org.aqaclient

import edu.umro.ScalaUtil.DicomCFind
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import edu.umro.ScalaUtil.Logging

object DicomFind extends Logging {

  private val done = scala.collection.mutable.HashSet[String]()

  /**
   * Build the C-FIND query and execute it.
   */
  def find(modality: String, patientID: String): Seq[AttributeList] = {
    val query = {
      val q = new AttributeList

      def add(tag: AttributeTag): Unit = {
        val a = AttributeFactory.newAttribute(tag)
        q.put(a)
      }

      def addWithValue(text: String, tag: AttributeTag): Unit = {
        val a = AttributeFactory.newAttribute(tag)
        a.addValue(text)
        q.put(a)
      }

      addWithValue(modality, TagFromName.Modality)
      addWithValue(patientID, TagFromName.PatientID)
      add(TagFromName.SeriesInstanceUID)
      q
    }

    done.synchronized({
      val resultList = DicomCFind.cfind(
        callingAETitle = ClientConfig.DICOMClient.aeTitle,
        calledPacs = ClientConfig.DICOMSource,
        attributeList = query,
        queryLevel = DicomCFind.QueryRetrieveLevel.SERIES,
        limit = None,
        queryRetrieveInformationModel = DicomCFind.QueryRetrieveInformationModel.StudyRoot)

      val msg = "C-FIND query PatientID: " + patientID + "    Modality: " + modality + "    number of results: " + resultList.size
      logger.info(msg) // TODO rm
      // TODO should do the following:
      //      if (!done.contains(msg)) {
      //        done += msg
      //        logger.info(msg)
      //      }
      resultList
    })
  }
}
