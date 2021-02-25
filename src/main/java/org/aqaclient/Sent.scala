package org.aqaclient

import java.util.Date

/**
 * Represent an upload set that has been sent to the AQA server.  In general, one attempt should
 * be made for each data set regardless of whether is succeeds or fails.  If it succeeds, then it
 * does not need to be sent again.  If it fails, then retrying probably won't help, and this client
 * could get stuck retrying the same bad data set over and over.
 *
 * The reason that the Results is not sufficient is the case of an occasional faulty data
 * set that the AQA refuses to process.  When uploaded, the AQA returns a failure status.  The data
 * set never shows up in the Results, so the client tries it over and over to the exclusion
 * of uploading anything else.
 *
 * A list of these is kept in memory, so a server restart will clear them out, and failed cases
 * will be re-sent.  There might also be a web interface in the future for re-sending a data set.
 */

case class Sent(uploadSet: Upload.UploadSet, msg: Option[String]) {
  val date = new Date
}

object Sent {

  /**
   * Pool of series whose DICOM contents have been fetched but have not yet been processed.
   */
  private val SentList = scala.collection.mutable.ArrayBuffer[Sent]()

  /**
   * Add a record to the list.
   */
  def add(sent: Sent): List[Sent] = SentList.synchronized {
    (SentList += sent).toList
  }

  /**
   * Return true if the list contains a series with the given series UID.
   */
  def hasImageSeries(SeriesInstanceUID: String): Boolean = SentList.synchronized {
    SentList.exists(sent => sent.uploadSet.imageSeries.SeriesInstanceUID.equals(SeriesInstanceUID))
  }

}