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

import edu.umro.RestletUtil.TrustKnownCertificates
import edu.umro.ScalaUtil.Logging
import java.security.cert.X509Certificate

/**
 * Initialize HTTPS certificate support.
 */
object HttpsInit extends Logging {

  def certToHuman(cert: X509Certificate): String = {
    cert.getSubjectDN.getName + "  " + "Valid from " + cert.getNotBefore + " to " + cert.getNotAfter
  }

  def init(): Unit = {
    val fileList = ClientConfig.certificateDir.listFiles
    if ((fileList == null) || fileList.isEmpty) {
      logger.warn("HTTPS will trust all server certificates regardless of their authenticity.  Put certs in " + ClientConfig.certificateDir.getAbsolutePath)
    } else {
      TrustKnownCertificates.init(fileList)
      val certList = TrustKnownCertificates.knownCerts
      if (certList.isEmpty)
        logger.warn("Could not find certificates.  HTTPS will trust all server certificates regardless of their authenticity.  Put certs in " + ClientConfig.certificateDir.getAbsolutePath)
      else
        logger.info("Number of X509 certificates: " + certList.length + certList.map(cert => certToHuman(cert)).mkString("\n", "\n", ""))
    }
  }
}