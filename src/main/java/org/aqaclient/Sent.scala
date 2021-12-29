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

case class Sent(uploadSet: DicomAssembleUpload.UploadSetDicomCMove, msg: Option[String]) {
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