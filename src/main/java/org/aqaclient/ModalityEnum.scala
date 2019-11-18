package org.aqaclient

/**
 * Provide static type and name of for modalities used in this service.
 */
object ModalityEnum extends Enumeration {
  val RTPLAN = Value
  val REG = Value
  val CT = Value
  val RTIMAGE = Value

  /**
   * Convert text to ModalityEnum.  Throw exception on failure to match.
   */
  def toModalityEnum(text: String) = {
    values.filter(v => v.toString.equalsIgnoreCase(text)).toSeq.head
  }
}