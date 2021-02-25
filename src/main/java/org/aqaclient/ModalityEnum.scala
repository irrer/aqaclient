package org.aqaclient

/**
 * Provide static type and name of for modalities used in this service.
 */
object ModalityEnum extends Enumeration {
  val RTPLAN: ModalityEnum.Value = Value
  val REG: ModalityEnum.Value = Value
  val CT: ModalityEnum.Value = Value
  val RTIMAGE: ModalityEnum.Value = Value

  /**
   * Convert text to ModalityEnum.  Throw exception on failure to match.
   */
  def toModalityEnum(text: String): ModalityEnum.Value = {
    values.filter(v => v.toString.equalsIgnoreCase(text)).toSeq.head
  }
}