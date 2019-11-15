package org.aqaclient.series

import java.io.File
import org.aqaclient.ClientConfig
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import edu.umro.ScalaUtil.Logging
import org.aqaclient.ClientUtil
import edu.umro.util.Utility
import org.aqaclient.DicomMove

/**
 * Describe a series whose DICOM has been retrieved but has not been processed.
 */
abstract case class Series(attributeList: AttributeList) extends Logging {
  def getString(tag: AttributeTag) = attributeList.get(tag).getStringValues.head
  val SeriesInstanceUID = getString(TagFromName.SeriesInstanceUID)
  val PatientID = getString(TagFromName.PatientID)
  val dir = new File(ClientConfig.tmpDir, SeriesInstanceUID)
}

object Series extends Logging {

  /**
   * Construct a series from an attribute list.
   */
  def constructSeries(attributeList: AttributeList): Series = {
    val modality = attributeList.get(TagFromName.Modality).getSingleStringValueOrEmptyString

    val series = modality.toUpperCase.trim match {
      case "RTPLAN" => new SeriesRtplan(attributeList)
      case "REG" => new SeriesReg(attributeList)
      case "CT" => new SeriesCt(attributeList)
      case "RTIMAGE" => new SeriesRtimage(attributeList)
      case _ => throw new RuntimeException("Unsupported modality: " + modality)
    }
    series
  }

  /**
   * Pool of series whose DICOM contents have been fetched but have not yet been processed.
   */
  private val SeriesPool = scala.collection.mutable.HashMap[String, Series]()

  def get(SeriesInstanceUID: String): Option[Series] = SeriesPool.synchronized({
    SeriesPool.get(SeriesInstanceUID)
  })

  def contains(SeriesInstanceUID: String) = get(SeriesInstanceUID).isDefined

  def put(series: Series) = SeriesPool.synchronized({
    SeriesPool.put(series.SeriesInstanceUID, series)
  })

  /**
   * Given a directory that contains the DICOM files of a series, reinstate the series.
   */
  private def reinstateSeries(dir: File) = {
    try {
      val al = ClientUtil.readDicomFile(dir.listFiles.head).right.get
      val series = constructSeries(al)
      put(series)
    } catch {
      case t: Throwable => logger.warn("Unexpected error while reading previously saved series from " + dir.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  private def reinststatePreviouslyFetechedSeries = {
    ClientConfig.tmpDir.listFiles.toList.filter(d => d.isDirectory).map(dir => reinstateSeries(dir))
  }

  //  /**
  //   * Remove temporary directories from previous run.
  //   */
  //  private def removeTmpDirs = {
  //    ClientConfig.tmpDir.listFiles.filter(dir => dir.getName.endsWith(DicomMove.tmpDirSuffix)).map(dir => Utility.deleteFileTree(dir))
  //  }

  def init = {
    //removeTmpDirs
    reinststatePreviouslyFetechedSeries
  }
}