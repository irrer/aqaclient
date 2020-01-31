package org.aqaclient

import edu.umro.RestletUtil.TrustKnownCertificates
import edu.umro.ScalaUtil.Logging
import java.security.cert.X509Certificate

/**
 * Initialize HTTPS certificate support.
 */
object HttpsInit extends Logging {

  def certToHuman(cert: X509Certificate) = {
    cert.getSubjectDN.getName + "  " + "Valid from " + cert.getNotBefore + " to " + cert.getNotAfter
  }

  def init = {
    val fileList = ClientConfig.certificateDir.listFiles
    if ((fileList == null) || (fileList.isEmpty)) {
      logger.warn("HTTPS will trust all server certificates regardless of their authenticity.  Put certs in " + ClientConfig.certificateDir.getAbsolutePath)
    } else {
      TrustKnownCertificates.init(fileList)
      val certList = TrustKnownCertificates.knownCerts
      if (certList.isEmpty)
        logger.warn("Could not find certificates.  HTTPS will trust all server certificates regardless of their authenticity.  Put certs in " + ClientConfig.certificateDir.getAbsolutePath)
      else
        logger.info("Number of X509 certificates: " + certList.size + certList.map(cert => certToHuman(cert)).mkString("\n", "\n", ""))
    }
  }
}