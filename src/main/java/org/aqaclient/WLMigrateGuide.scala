package org.aqaclient

import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.Trace

import scala.xml.XML

/**
 * Show what WL data has been migrated to prod and which has not.
 */
object WLMigrateGuide {
  private def getPatientIdList: Seq[String] = {
    // @formatter:off
    //noinspection SpellCheckingInspection
    Seq(
      "$QASRSWL_TB3", "$QASRSWL_TX6", "$QASRSWL02ISOCAL00", "$QASRSWL2P5MV", "$QASRSWLBALL2023APR", "$QASRSWLBALL2023MAR", "$QASRSWLBALLDICOM-RT00", "$QASRSWLBALLMILL00", "$QASRSWLBALLTB5SEP2022", "$QASRSWLBALLTX3SEP2022", "$QASRSWLOSMS", "$QASRSWLTB5082020", "QASRSWL00ISOCAL00", "QASRSWL00ISOCAL01", "QASRSWL01ISOCAL00", "QASRSWL1", "QASRSWL2", "QASRSWL20140607", "QASRSWL8G6T2C2020", "QASRSWLBALL0003",
      "QASRSWLBALL1604", "QASRSWLBALL1607", "QASRSWLBALL1610", "QASRSWLBALL2017AUG", "QASRSWLBALL2017FEB", "QASRSWLBALL2017JUN", "QASRSWLBALL2017OCT", "QASRSWLBALL2018APR", "QASRSWLBALL2018JAN", "QASRSWLBALL2018JUL", "QASRSWLBALL2018MAR", "QASRSWLBALL2018MAY", "QASRSWLBALL2018OCT", "QASRSWLBALL2018SEP", "QASRSWLBALL2019APR", "QASRSWLBALL2019AUG", "QASRSWLBALL2019FEB", "QASRSWLBALL2019JAN", "QASRSWLBALL2019JUN", "QASRSWLBALL2019MAR",
      "QASRSWLBALL2019NOV", "QASRSWLBALL2020JAN", "QASRSWLBALL2020JUNE", "QASRSWLBALL2020MAR", "QASRSWLBALL2020MAY", "QASRSWLBALL2020NOV", "QASRSWLBALL2020SEP", "QASRSWLBALL2021AUG", "QASRSWLBALL2021DEC", "QASRSWLBALL2021JAN", "QASRSWLBALL2021MAR", "QASRSWLBALL2021MAY", "QASRSWLBALL2021OCT", "QASRSWLBALL2022DEC", "QASRSWLBALL2022FEB", "QASRSWLBALL2022JUN", "QASRSWLBallTBAug2022", "QASRSWLCBCT_BR1", "QASRSWLCBCT_TB3", "QASRSWLCBCT1",
      "QASRSWLCBCT2017Q3", "QASRSWLCBCT2017Q4", "QASRSWLCBCT2018Q1", "QASRSWLCBCT2018Q3", "QASRSWLCBCT2018Q4", "QASRSWLCBCT2019AUG", "QASRSWLCBCT2019FEB", "QASRSWLCBCT2019MAY", "QASRSWLCBCT2019Q1", "QASRSWLCBCT2020APR", "QASRSWLCBCT2020AUG", "QASRSWLCBCT2020Feb", "QASRSWLCBCT2020Jan", "QASRSWLCBCT2020NOV", "QASRSWLCBCT2021AUG", "QASRSWLCBCT2021FEB", "QASRSWLCBCT2021MAY", "QASRSWLCBCT2021NOV", "QASRSWLCTCT2", "QASRSWLISOCAL2022AUG",
      "QASRSWLISOCAL2022FEB", "QASRSWLTB7C", "QASRSWLTB8G", "QASRSWLTBISOCAL16G", "QASRSWLTX4"
    )
    // @formatter:on
    //noinspection SpellCheckingInspection
    Seq("$QASRSWLBALLMILL00", "QASRSWL8G6T2C2020", "QASRSWLBALL2019NOV")
  }

  private def seriesListToSeriesUidList(text: String): Seq[String] = {
    val e = XML.loadString(text)

    val nodeList = (e \ "Series").filter(n => (n \ "Modality").head.text.equals("RTIMAGE"))

    val uidList = nodeList.map(n => (n \ "SeriesInstanceUID").head.text)
    uidList
  }

  private def hasSlices(seriesUid: String): Boolean = {
    val sliceUidList = DicomFind.getSliceUIDsInSeries(seriesUid)
    sliceUidList.nonEmpty
  }

  /**
   * Return the number of RTIMAGE series that are in ARIA but on AQA.
   *
   * @param patientID For this patient.
   * @return The number of RTIMAGE series that are in ARIA but on AQA.
   */
  private def show(patientID: String): Int = {
    Thread.sleep(5 * 1000)
    Trace.trace(s"$patientID starting ...")
    val ariaSeriesList = {
      val ariaList = DicomFind.find("RTIMAGE", patientID)
      val attrList = ariaList.map(_.get(TagByName.SeriesInstanceUID))
      val uidList = attrList.map(_.getSingleStringValueOrEmptyString()).sorted
      uidList.filter(hasSlices)
    }
    val aqaText = Results.getHttpTextFromServer(patientID)

    if (aqaText.isDefined) {
      val aqaSeriesList = seriesListToSeriesUidList(aqaText.get).sorted

      val todoList = ariaSeriesList.diff(aqaSeriesList).sorted

      Trace.trace(s"$patientID ARIA size: ${ariaSeriesList.size}")
      Trace.trace(s"$patientID ARIA list: $ariaSeriesList")

      Trace.trace(s"$patientID AQA  size: ${aqaSeriesList.size}")
      Trace.trace(s"$patientID AQA  list: $aqaSeriesList")

      Trace.trace(s"$patientID todo size: ${todoList.size}")
      Trace.trace(s"$patientID todo list: $todoList")

      /*
      def getEm(seriesUID: String): Unit = {
        val series = DicomMove.get(seriesUID, patientID, Modality = "RTIMAGE")
        Trace.trace(s"Got series $series")
        Trace.trace()
      }

      todoList.foreach(getEm)
      */

      todoList.size
    }
    else {
      Trace.trace(s"No AQA text for $patientID")
      -1
    }
  }

  def main(args: Array[String]): Unit = {
    Trace.trace()
    ClientConfig.validate
    Trace.trace()
    HttpsInit.init()
    Trace.trace()
    DicomMove.init()
    Trace.trace()
    val start = System.currentTimeMillis()


    val patientIdList = getPatientIdList

    patientIdList.map(show)

    val elapsed = System.currentTimeMillis() - start
    println(s"Done.  Elapsed time in ms: $elapsed")
    Thread.sleep(1000)
    System.exit(0)
  }

}
