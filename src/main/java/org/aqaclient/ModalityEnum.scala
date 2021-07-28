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