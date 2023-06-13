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

import edu.umro.ScalaUtil.FileUtil
import org.aqaclient.ClientConfig
import org.aqaclient.PatientProcedure
import org.aqaclient.Results
import org.scalatest.FlatSpec
import org.scalatest.Matchers

import java.io.File
import scala.xml.XML

/**
  * Test the Results.procedureOfPlanWithFrameOfReferenceUID function.
  */

class TestResultsProcOfPlan extends FlatSpec with Matchers {
  "Client Configuration" should "define values" in {
    ClientConfig.validate

    println("======================= Starting test. =======================")
    val file = new File("""src\test\resources\TestResultsProcOfPlan.xml""")

    val elem = XML.loadFile(file)

    val patientId = "patient"

    Results.testPut(patientId, elem)

    val ppFile = new File("""src\test\resources\PatientProcedureList.xml""")
    val ppText = FileUtil.readTextFile(ppFile).right.get
    PatientProcedure.populateFromText(ppText)

    val s1 = Results.procedureOfPlanWithFrameOfReferenceUID(patientId, "1.2.246.352.205.4839200455795585025.4195600589068878499")
    println(s"==== s1: $s1")
    s1.isDefined should be(true)

    val s2 = Results.procedureOfPlanWithFrameOfReferenceUID(patientId, "1.3.6.1.4.1.22361.17483834219463.64289797.1623239465285.351")
    println(s"==== s2: $s2")
    s2.isDefined should be(true)

    val s3 = Results.procedureOfPlanWithFrameOfReferenceUID(patientId, "1.2.246.352.62.3.5427220765397235523.18010771099079258756")
    println(s"==== s3: $s3")
    s3.isDefined should be(false)

    println("done")
  }
}
