package aqaclient.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.aqaclient.ClientConfig
import org.aqaclient.Results
import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.Trace

/**
 * Test the Config.
 */

class TestHTTPS extends FlatSpec with Matchers {
  "TestHTTPS" should "define values" in {

    val PatientID = "MQATX4OBIQA2019Q3" //  "$tx1-OBI10^OBIQA2019Q4"
    val baseUrl = "https://uhroappwebsdv1.umhs.med.umich.edu:8111"
    val url = baseUrl + "/GetSeries?PatientID=" + PatientID
    Trace.trace(url)
    val user = "irrer"
    val password = "23eetslp"
    val elem = HttpsClient.httpsGet(url, ClientConfig.AQAUser, ClientConfig.AQAPassword)
    Trace.trace(elem)
    true should be(true)
  }
}
