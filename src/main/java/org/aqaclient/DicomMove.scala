package org.aqaclient

import edu.umro.ScalaUtil.DicomReceiver
import com.pixelmed.network.ReceivedObjectHandler
import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.Logging
import scala.collection.mutable.ArrayBuffer
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.SOPClass
import java.io.File
import edu.umro.util.Utility

object DicomMove extends Logging {

  private val dicomList = ArrayBuffer[File]()

  val activeDirName = "active"

  private class MyReceivedObjectHandler extends ReceivedObjectHandler {
    override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String) = {
      val file = new File(ClientConfig.tmpDir, fileName)
      dicomList += file
      logger.info("Received file " + file.getAbsolutePath)
    }
  }

  private lazy val dicomReceiver = {
    logger.info("Starting DicomReceiver ...")
    val dr = new DicomReceiver(ClientConfig.tmpDir, ClientConfig.DICOMClient, new MyReceivedObjectHandler)
    Utility.deleteFileTree(dr.setSubDir(activeDirName))
    logger.info("Started DicomReceiver.  This DICOM connection: " + ClientConfig.DICOMClient)
    dr
  }

  /**
   * Get all files for the given series.  On failure return an empty list and log an error message.
   */
  def get(SeriesInstanceUID: String): List[File] = dicomList.synchronized({
    //val subDir = new File(ClientConfig.tmpDir, SeriesInstanceUID + tmpDirSuffix)
    //dicomReceiver.setSubDir(SeriesInstanceUID + tmpDirSuffix)
    Utility.deleteFileTree(dicomReceiver.getSubDir)
    dicomList.clear
    val specification = new AttributeList

    def addAttr(tag: AttributeTag, value: String): Unit = {
      val a = AttributeFactory.newAttribute(tag)
      a.addValue(value)
      specification.put(a)
    }

    addAttr(TagFromName.QueryRetrieveLevel, "SERIES")
    addAttr(TagFromName.SeriesInstanceUID, SeriesInstanceUID)

    dicomReceiver.cmove(specification, ClientConfig.DICOMSource, ClientConfig.DICOMClient, SOPClass.PatientRootQueryRetrieveInformationModelMove) match {
      case Some(msg) => logger.error("C-MOVE failed: " + msg)
      case _ =>
    }

    // Note: the 'toList' preserves the contents of the ArrayBuffer, unlike toSeq.
    val list = dicomList.toList
    dicomList.clear
    list
  })

  def init = {
    // force dicomReceiver to be instantiated
    dicomReceiver.mainDirName
  }
}
