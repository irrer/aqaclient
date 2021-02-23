package aqaclient.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.aqaclient.ClientConfig
import org.aqaclient.Results
import org.aqaclient.DicomMove

/**
 * Test getting DICOM files.
 */

class TestDicomGet extends FlatSpec with Matchers {
  "get" should "get DICOM" in {

    case class TestData(seriesInstanceUID: String, valid: Boolean) {
      def get = {
        val series = DicomMove.get(seriesInstanceUID, "expected to succeed: " + valid)

        ((series.isDefined && valid) || (series.isEmpty && (!valid))) should be(true)
      }
      override def toString = "seriesInstanceUID: " + seriesInstanceUID + "    valid: " + valid
    }

    val testDataSeq = Seq(
      TestData("1.2.246.352.62.2.5481191684122312133.2120842094365118854", true),
      TestData("1.2.246.352.62.2.000000000000000.000000000000000", false))

    ClientConfig.validate
    ClientConfig.validated should be(true)

    // Note that a PACS (ConQuest?) device must be running and have the appropriate test data.
    if (false) // remove this to run the test
      testDataSeq.map(t => t.get)
    else
      println("This test has been disabled.")
  }
}
