/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqaclient

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import org.aqaclient

case class RtplanType(planType: RtplanType.Value, keywordList: Seq[String]) {
  override def toString: String = planType.toString + " : " + keywordList.mkString("  ||  ")

  /**
    * Return true if the label matches this plan type.
    */
  def matches(rtplanLabel: String): Boolean = {
    keywordList.exists(kw => kw.toLowerCase.contains(rtplanLabel.toLowerCase))
  }
}

/**
  * Provide static type and name of for modalities used in this service.
  */
object RtplanType extends Enumeration {
  val Phase2: aqaclient.RtplanType.Value = Value
  val DailyQA: aqaclient.RtplanType.Value = Value
  val LOC: aqaclient.RtplanType.Value = Value
  val LOCBaseline: aqaclient.RtplanType.Value = Value
  val WinstonLutz: aqaclient.RtplanType.Value = Value
  val GapOffsetSkew: aqaclient.RtplanType.Value = Value

  /**
    * Convert text to RtplanType.  Throw exception on failure to match.
    */
  def toRtplanType(text: String): aqaclient.RtplanType.Value = {
    values.filter(v => v.toString.equalsIgnoreCase(text)).toSeq.head
  }

  /**
    * Get the plan type of an attribute list.
    */
  def planType(al: AttributeList): Either[String, RtplanType] = {
    val label = {
      val a = al.get(TagByName.RTPlanLabel)
      if (a == null)
        "No RTPLAN Label"
      else
        a.getSingleStringValueOrEmptyString
    }

    val list = ClientConfig.rtplanTypeList.filter(pt => pt.matches(label))

    list.size match {
      case 1 => Right(list.head)
      case 0 => Left("No rtplan types match rtplan label " + label)
      case _ => Left("More that one rtplan types match rtplan label " + label + " :\n    " + list.mkString("\n    "))
    }
  }
}
