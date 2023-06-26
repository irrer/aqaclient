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

import edu.umro.RestletUtil.HttpsClient
import edu.umro.ScalaUtil.Trace
import org.restlet.data.ChallengeScheme
import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
  * Test the Config.
  */

class TestHTTPS extends FlatSpec with Matchers {
  "TestHTTPS" should "define values" in {

    val PatientID = "MQATX4OBIQA2019Q3" //  "$tx1-OBI10^OBIQA2019Q4"
    val baseUrl = "https://uhroappwebsdv1.umhs.med.umich.edu:8111"
    val url = baseUrl + "/GetSeries?PatientID=" + PatientID
    Trace.trace(url)
    val userId = "irrer"
    val password = "23eetslp"

    val cs = HttpsClient.makeChallengeResponse(scheme = ChallengeScheme.HTTP_BASIC, userId, password)

    val cr = HttpsClient.makeClientResource(url, challengeResponse = Some(cs), trustKnownCertificates = true)

    val elem = HttpsClient.httpsGet(cr, url)
    Trace.trace(elem)
    true should be(true)
  }
}
