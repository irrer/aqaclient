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
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.dicomCFind.DicomCFindInstancesForSeries
import edu.umro.ScalaUtil.dicomCFind.DicomCFindSeriesForPatient

/**
  * Support C-FIND calls for this service.
  */
object DicomFind extends Logging {

  private var dicomCFindSeriesForPatient: Option[DicomCFindSeriesForPatient] = None

  private def getDicomCFindSeriesForPatient: DicomCFindSeriesForPatient = {
    if (dicomCFindSeriesForPatient.isEmpty)
      dicomCFindSeriesForPatient = Some(new DicomCFindSeriesForPatient(ClientConfig.DICOMClient.aeTitle, ClientConfig.DICOMSource))

    dicomCFindSeriesForPatient.get
  }

  private var dicomCFindInstancesForSeries: Option[DicomCFindInstancesForSeries] = None

  /**
   * Get a list of SOPInstanceUIDs for the given series.
   *
   * <p><em><b>
   * NOTE: A real 'GOTCHA' is that if the retrieve list just asks for the SOPInstanceUID, then it always works for Varian VMSDBD.  But, if
   * there are extra values requested, and a series of modality REG is being queried, then it sometimes returns zero entries.
   * </b></em></p>
   *
   * <p><em><b>
   * This is a bug in the Varian VMSDBD.
   * </b></em></p>
   *
   * @return
   */
  private def getDicomCFindInstancesForSeries = {
    if (dicomCFindInstancesForSeries.isEmpty) {
      dicomCFindInstancesForSeries = Some(new DicomCFindInstancesForSeries(ClientConfig.DICOMClient.aeTitle, ClientConfig.DICOMSource, retrieveList = Seq(TagByName.SOPInstanceUID)))
    }

    dicomCFindInstancesForSeries.get
  }

  /**
    * Perform a C-FIND query that gets a list of series of the given modality for the given patient.
    */
  def findSeriesForPatientOfModality(Modality: String, PatientID: String): Seq[AttributeList] = {
    def dicomOp: Seq[AttributeList] = {
      val list = getDicomCFindSeriesForPatient.findSeriesForPatient(PatientID, Some(Modality))
      list
    }

    val description = s"C-FIND for $PatientID for all series of modality: $Modality"
    val list = {
      DicomSemaphore.processInSemaphore(dicomOp _, getDicomCFindSeriesForPatient.close _, description)
    }
    logger.info(s"$description returned ${list.size} series.")
    list
  }

  /**
    * Get the list of SOPInstanceUIDs for the given series.
    *
    * @param SeriesInstanceUID For this series.
    *
    * @return List of instance (slice) UIDs
    */
  def getSliceUIDsInSeries(SeriesInstanceUID: String, PatientID: String, Modality: String): Seq[String] = {
    def dicomOp: Seq[AttributeList] = {
      val list = getDicomCFindInstancesForSeries.findInstanceListForSeries(SeriesInstanceUID)
      list
    }

    val description = s"C-FIND for image list in series $SeriesInstanceUID    $PatientID    $Modality"
    val list = DicomSemaphore.processInSemaphore(dicomOp _, getDicomCFindInstancesForSeries.close _, description)
    logger.info(s"$description returned ${list.size} instances.")
    list.map(_.get(TagByName.SOPInstanceUID).getSingleStringValueOrEmptyString)
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

    println("\n\n\n\n\n\n\n\n============================================")

    if (true) {
      val PatientID = "$TB4_OBI_2024"
      val Modality = "REG"
      //val imageList = getSliceUIDsInSeries("1.2.246.352.62.2.4731927922401186236.462869341920626072", PatientID, Modality)
      val imageList = getSliceUIDsInSeries("1.2.246.352.62.2.4613414455814797398.11320105932616602785", PatientID, Modality)
      println(s"image list size: ${imageList.size}")
      println("\n" + imageList.mkString("\n"))
    }

    println("\n\n\n\n")

    val PatientID = "$TB5_OBI_2022Q2"
    val Modality = "CT"
    val imageList = getSliceUIDsInSeries("1.2.246.352.62.2.4933051009168731539.6682484753785086647", PatientID, Modality)
    println(s"image list size: ${imageList.size}")
    println("\n" + imageList.mkString("\n"))

    println("\n\n\n\n")
    val seriesList = findSeriesForPatientOfModality(Modality, PatientID)
    println(s"series list size: ${seriesList.size}")
    println(s"first series found:\n${seriesList.head}")
    println("============================================")
    println("Exiting ...")
    System.exit(99)

  }
}
