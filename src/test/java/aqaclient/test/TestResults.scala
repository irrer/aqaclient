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

package aqaclient.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.aqaclient.ClientConfig
import org.aqaclient.Results

/**
 * Test the Config.
 */

class TestResults extends FlatSpec with Matchers {
  "Client Configuration" should "define values" in {

    case class TestData(patientId: String, seriesInstanceUID: String, expected: Boolean) {
      override def toString = "patientId: " + patientId + "    seriesInstanceUID: " + seriesInstanceUID + "    expected: " + expected
    }

    val testList = Seq(
      new TestData("MQATX1OBIQA2019Q3", "1.2.246.352.62.2.5481191684122312133.2120842094365118854", true),
      new TestData("noSuchPatient", "1.2.246.352.62.2.000000000000000.000000000000000", false))
    for (tst <- testList) {
      val doesContain = Results.containsSeries(tst.patientId, tst.seriesInstanceUID)
      println("Contains series:  " + tst + "    actual: " + doesContain)
      tst.expected should be(doesContain)
    }

    ClientConfig.validate
    ClientConfig.validated should be(true)
  }
}
