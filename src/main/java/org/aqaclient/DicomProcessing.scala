package org.aqaclient

import edu.umro.ScalaUtil.DicomCFind
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import edu.umro.ScalaUtil.Logging

object DicomProcessing extends Logging {

  /**
   * Return true if the series has already been processed.
   */
  private def processed(al: AttributeList): Boolean = {
    val SeriesInstanceUID = al.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrNull
    ProcessedSeries.get(SeriesInstanceUID).isDefined
  }

  private def getSeries(SeriesInstanceUID: String): Seq[AttributeList] = {
    ???
  }

  /**
   * Look for new files to process.
   */
  private def updatePatient(patientID: String) = {

    val planList = DicomFind.find("RTPLAN", patientID).filterNot(processed _)

    val planAlList = planList.map(plan => getSeries(plan.get(TagFromName.SeriesInstanceUID).getSingleStringValueOrNull))
  }

  def update = {
    PatientIDList.getPatientIDList.map(patientID => updatePatient(patientID))
  }

  def init = {
    update

  }
}
