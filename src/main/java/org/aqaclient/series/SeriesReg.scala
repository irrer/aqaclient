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
  val RegFrameOfReferenceUID = {
    val RegistrationSequence = DicomUtil.seqToAttr(attributeList, TagFromName.RegistrationSequence)
    val ReferencedImageSequence = RegistrationSequence.map(rs => DicomUtil.seqToAttr(rs, TagFromName.ReferencedImageSequence)).flatten
    val frmOfRef = ReferencedImageSequence.map(ris => ris.get(TagFromName.FrameOfReferenceUID).getStringValues.head).filterNot(frmOfRef => frmOfRef.equals(FrameOfReferenceUID)).head
    frmOfRef
  }
}