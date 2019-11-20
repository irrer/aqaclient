package org.aqaclient

import edu.umro.ScalaUtil.Logging
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import edu.umro.ScalaUtil.Trace

/**
 * Group series into sets of data that can be processed and upload to the AQA platform.
 */

class Upload extends Actor with Logging {
  override def receive = {
    case notification: Any => {
      logger.info("Upload received notification of new Series")
      Upload.update
    }
  }
}

object Upload {

  private def searchForCtWithoutReg = {
    val ctList = Series.getByModality(ModalityEnum.CT).sortBy(s => s.dataDate)
    
  }

  private def searchForCtWithReg = Trace.trace("TODO") // TODO
  private def searchForRtimage = Trace.trace("TODO") // TODO

  /**
   * Look for test-ready sets of series and upload them.
   */
  private def update = {
    Trace.trace("TODO") // TODO
    searchForCtWithoutReg
    searchForCtWithReg
    searchForRtimage
  }

  private val system = ActorSystem("Upload")
  private val uploadActor = system.actorOf(Props[Upload], name = "uploadActor")

  /**
   * Indicate that new data is available for processing.  It may or may not be sufficient to
   * create a data set for processing.  This function sends a message to the Upload actor and
   * returns immediately.
   */
  def scanSeries = {
    uploadActor ! "update"
  }

  def init = {
    update
  }

}
