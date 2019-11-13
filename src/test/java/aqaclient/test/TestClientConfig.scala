
package aqaclient.test;

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.aqaclient.ClientConfig

/**
 * Test the Config.
 *
 */

class TestClientConfig extends FlatSpec with Matchers {
  "Client Configuration" should "define values" in {
    ClientConfig.validate
    ClientConfig.validated should be(true)
  }
}
