package org.aqaclient

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName

case class RtplanType(planType: RtplanType.Value, keywordList: Seq[String]) {
  override def toString = planType.toString + " : " + keywordList.mkString("  ||  ")

  /**
   * Return true if the label matches this plan type.
   */
  def matches(rtplanLabel: String): Boolean = {
    keywordList.filter(kw => kw.toLowerCase.contains(rtplanLabel.toLowerCase)).nonEmpty
  }
}

/**
 * Provide static type and name of for modalities used in this service.
 */
object RtplanType extends Enumeration {
  val Phase2 = Value
  val DailyQA = Value
  val LOC = Value
  val LOCBaseline = Value

  /**
   * Convert text to RtplanType.  Throw exception on failure to match.
   */
  def toRtplanType(text: String) = {
    values.filter(v => v.toString.equalsIgnoreCase(text)).toSeq.head
  }

  /**
   * Get the plan type of an attribute list.
   */
  def planType(al: AttributeList): Either[String, RtplanType] = {
    val label = {
      val a = al.get(TagFromName.RTPlanLabel)
      if (a == null)
        "No RTPLAN Label"
      else
        a.getSingleStringValueOrEmptyString
    }

    val list = ClientConfig.rtplanTypeList.filter(pt => pt.matches(label))

    list.size match {
      case 1 => Right(list.head)
      case 0 => Left("No rtplan types match rtplanlabel " + label)
      case _ => Left("More that one rtplan types match rtplanlabel " + label + " :\n    " + list.mkString("\n    "))
    }
  }
}