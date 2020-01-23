package aqaclient.test

import edu.umro.ScalaUtil.DicomCFind
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.PACS

object VerifyDicomIsArchived extends Logging {

  //val calledPacs = new PACS("VMSDBD", "10.30.65.100", 105)
  val calledPacs = new PACS("ROBRCTSYNGO", "10.20.230.82", 104)

  private def makeQuery(list: Seq[Any]): AttributeList = {
    val al = new AttributeList

    def addAttr(s: Any) = {
      if (s.isInstanceOf[AttributeTag]) {
        val a = AttributeFactory.newAttribute(s.asInstanceOf[AttributeTag])
        al.put(a)
      } else {
        val ss = s.asInstanceOf[(AttributeTag, String)]
        val a = AttributeFactory.newAttribute(ss._1.asInstanceOf[AttributeTag])
        a.addValue(ss._2)
        al.put(a)
      }
    }

    list.map(s => addAttr(s))
    al
  }

  /**
   * Get the list of all series.
   */
  private def getSeriesList = {
    val query = makeQuery(Seq(
      TagFromName.PatientID,
      TagFromName.PatientName,
      TagFromName.SeriesDate,
      TagFromName.SeriesTime,
      TagFromName.StudyDate,
      TagFromName.StudyTime,
      TagFromName.Modality,
      TagFromName.SeriesDescription,
      TagFromName.StudyDescription,
      TagFromName.StudyInstanceUID,
      TagFromName.SeriesInstanceUID))

    val resultList = DicomCFind.cfind(
      callingAETitle = "IRRER",
      calledPacs,
      attributeList = query,
      queryLevel = DicomCFind.QueryRetrieveLevel.SERIES,
      limit = None,
      queryRetrieveInformationModel = DicomCFind.QueryRetrieveInformationModel.StudyRoot)

    resultList
  }

  private def getPatientList = {
    val query = makeQuery(Seq(TagFromName.PatientID, TagFromName.PatientName))
    val resultList = DicomCFind.cfind(
      callingAETitle = "IRRER",
      calledPacs,
      attributeList = query,
      queryLevel = DicomCFind.QueryRetrieveLevel.SERIES,
      limit = None,
      queryRetrieveInformationModel = DicomCFind.QueryRetrieveInformationModel.StudyRoot)

    def alToMap(al: AttributeList) = {
      val id = al.get(TagFromName.PatientID).getSingleStringValueOrEmptyString
      val name = al.get(TagFromName.PatientName).getSingleStringValueOrEmptyString
      (id, name)
    }

    resultList.map(al => alToMap(al)).toMap.toSeq.sorted
  }

  private def getSeriesList(PatientID: String) = {
    val query = makeQuery(Seq((TagFromName.PatientID, PatientID)))
    val resultList = DicomCFind.cfind(
      callingAETitle = "IRRER",
      calledPacs,
      attributeList = query,
      queryLevel = DicomCFind.QueryRetrieveLevel.SERIES,
      limit = None,
      queryRetrieveInformationModel = DicomCFind.QueryRetrieveInformationModel.StudyRoot)
  }

  /**
   * Determine the number of slices in the given series.
   */
  private def getSliceCount(SeriesInstanceUID: String): Int = {
    val resultList = DicomCFind.cfind(
      callingAETitle = "IRRER",
      calledPacs,
      attributeList = makeQuery(Seq((TagFromName.SeriesInstanceUID, SeriesInstanceUID))),
      queryLevel = DicomCFind.QueryRetrieveLevel.IMAGE,
      limit = None,
      queryRetrieveInformationModel = DicomCFind.QueryRetrieveInformationModel.StudyRoot)

    println("number of results: " + resultList.size)

    resultList.size
  }

  private def makeSeriesReport(series: AttributeList) = {
    def gt(tag: AttributeTag) = {
      val at = series.get(tag)
      if (at == null) ""
      else at.getSingleStringValueOrEmptyString.trim.replace('\0', ' ')
    }
    val slices = getSliceCount(gt(TagFromName.SeriesInstanceUID))
    println("        " + gt(TagFromName.SeriesDescription) + "  " + gt(TagFromName.Modality) + "  " + gt(TagFromName.SeriesDate) + "  " + gt(TagFromName.SeriesTime) + "  Slices: " + slices)
  }

  private def makeStudyReport(seriesList: Seq[AttributeList]) = {
    val first = seriesList.head
    def gt(tag: AttributeTag) = {
      val at = first.get(tag)
      if (at == null) ""
      else at.getSingleStringValueOrEmptyString.trim.replace('\0', ' ')
    }
    println(
      "    Study " + gt(TagFromName.StudyDescription) +
        "  " + gt(TagFromName.StudyDate) +
        "  " + gt(TagFromName.StudyTime) + "  Number of series: " + seriesList.size)
    seriesList.map(series => makeSeriesReport(series))
  }

  private def makePatientReport(PatientID: String, seriesListAll: Seq[AttributeList]): Unit = {
    val seriesList: Seq[AttributeList] = seriesListAll.filter(s => s.get(TagFromName.PatientID).getSingleStringValueOrEmptyString.trim.equalsIgnoreCase(PatientID.trim))
    val PatientName: String = {
      seriesList.head.get(TagFromName.PatientName).getSingleStringValueOrEmptyString.trim.replace('\0', ' ')
    }

    val studyList = seriesList.groupBy(s => s.get(TagFromName.StudyInstanceUID).getSingleStringValueOrEmptyString.trim)
    println("\nPatient: " + PatientID + " : " + PatientName + "    Number of Studies: " + studyList.size + "    Number of Series: " + seriesList.size)
    studyList.map(study => makeStudyReport(study._2))
  }

  def main(args: Array[String]): Unit = {
    println("Starting...")
    val start = System.currentTimeMillis
    val seriesList = getSeriesList
    println("Patient List:")
    val PatientIDList = seriesList.map(s => s.get(TagFromName.PatientID).getSingleStringValueOrEmptyString).distinct.sorted
    PatientIDList.map(p => println(p.formatted("%20s")))

    PatientIDList.map(PatientID => makePatientReport(PatientID, seriesList))

    val elapsed = System.currentTimeMillis - start
    println("Done.   Elapsed ms: ")
  }

}
