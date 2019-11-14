package org.aqaclient.series

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName

class SeriesRtplan(attributeList: AttributeList) extends Series(attributeList) {
  val FrameOfReferenceUID = getString(TagFromName.FrameOfReferenceUID)
}