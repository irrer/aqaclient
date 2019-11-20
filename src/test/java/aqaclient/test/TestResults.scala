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
