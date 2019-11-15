package org.aqaclient.series

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil

/**
 * Describe an image registration series.
 */
class SeriesReg(attributeList: AttributeList) extends Series(attributeList) {
  /** Frame of reference for this registration. */
  val FrameOfReferenceUID = getString(TagFromName.FrameOfReferenceUID)

  /** Frame of reference that this can register to. */
  val RegFrameOfReferenceUID: String = {
    DicomUtil.findAllSingle(attributeList, TagFromName.FrameOfReferenceUID).
      map(a => a.getSingleStringValueOrEmptyString).
      distinct.
      filterNot(frmRef => frmRef.equals(FrameOfReferenceUID)).
      head
  }
}