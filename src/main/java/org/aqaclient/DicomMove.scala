package org.aqaclient

import edu.umro.ScalaUtil.DicomReceiver
import com.pixelmed.network.ReceivedObjectHandler
import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.Logging
import scala.collection.mutable.ArrayBuffer
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.AttributeFactory

object DicomMove extends Logging {

  private val dicomList = ArrayBuffer[AttributeList]()

  private class MyReceivedObjectHandler extends ReceivedObjectHandler {
    override def sendReceivedObjectIndication(fileName: String, transferSyntax: String, callingAETitle: String) = {
      logger.info("Received file " + fileName)
    }
  }

  private lazy val dicomReceiver = {
    logger.info("Started DicomReceiver")
    val dr = new DicomReceiver(ClientConfig.tmpDir, ClientConfig.DICOMClient, new MyReceivedObjectHandler)
    logger.info("Started DicomReceiver: " + ClientConfig.DICOMClient)
    dr
  }

  /**
   * Get all files for the given series.  On failure return an empty list and log an error message.
   */
  def get(SeriesInstanceUID: String): List[AttributeList] = dicomList.synchronized({

    dicomList.clear
    val specification = new AttributeList

    def addAttr(tag: AttributeTag, value: String): Unit = {
      val a = AttributeFactory.newAttribute(tag)
      a.addValue(value)
      specification.put(a)
    }

    addAttr(TagFromName.QueryRetrieveLevel, "SERIES")
    addAttr(TagFromName.SeriesInstanceUID, SeriesInstanceUID)

    dicomReceiver.cmove(specification, ClientConfig.DICOMSource, ClientConfig.DICOMClient) match {
      case Some(msg) => logger.error("C-MOVE failed: " + msg)
      case _ =>
    }

    // Note: the 'toList' preserves the contents of the ArrayBuffer, unlike toSeq.
    val list = dicomList.toList
    dicomList.clear
    list
  })

}
