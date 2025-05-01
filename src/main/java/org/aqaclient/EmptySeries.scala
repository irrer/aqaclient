package org.aqaclient

import edu.umro.ScalaUtil.Logging

import java.text.SimpleDateFormat
import java.util.Date

/*
 * Copyright 2024 Regents of the University of Michigan
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

private case class EmptySeriesHistory(date: Date, size: Int) {

  val dateFormat = new SimpleDateFormat("yyyy MM dd HH:mm:ss.SSS")
  override def toString: String = {
    val d = dateFormat.format(date)
    val s = size.formatted("%3d")
    s"$d : $s"
  }
}

case class EmptySeries(SeriesInstanceUID: String, PatientID: String, Modality: String) {
  private val history = scala.collection.mutable.ArrayBuffer[EmptySeriesHistory]()

  def append(size: Int): Unit = history.append(EmptySeriesHistory(new Date, size))

  private def elapsed_ms = {
    val all = history.map(_.date.getTime)
    all.max - all.min
  }

  override def toString: String = {
    s"Series: $SeriesInstanceUID     $PatientID    $Modality    Elapsed: ${edu.umro.ScalaUtil.Util.intervalTimeUserFriendly(elapsed_ms)}\n    " + history.mkString("\n    ")
  }

}

/**
  * When getting series from Varian, we first do a C-FIND to get a list of slices before doing the
  * C-MOVE.  Occasionally this returns zero slices.  This module keeps track of series that behave
  * this way.  The best hypothesis is that Varian locks the series/patient while in treatment.
  */

object EmptySeries extends Logging {

  private val emptySeriesList = scala.collection.mutable.HashMap[String, EmptySeries]()

  /**
    * Return true if C-FIND returns zero slices.  Also keep track of such series.
    * @param SeriesInstanceUID Series UID.
    * @param PatientID Patient ID for logging.
    * @param Modality Modality ID for logging.
    * @return True if no slices.
    */
  def seriesIsEmpty(SeriesInstanceUID: String, PatientID: String, Modality: String): Boolean = {
    val sliceList = DicomFind.getSliceUIDsInSeries(SeriesInstanceUID, PatientID, Modality)

    emptySeriesList.synchronized {
      if (emptySeriesList.contains(SeriesInstanceUID)) {
        emptySeriesList(SeriesInstanceUID).append(sliceList.size)
        logger.warn(s"Empty series activity. size: ${emptySeriesList.size}  SeriesInstanceUID: $SeriesInstanceUID  list contains it: ${emptySeriesList.contains(SeriesInstanceUID)}")
      } else {
        if (sliceList.isEmpty) {
          val es = EmptySeries(SeriesInstanceUID, PatientID, Modality)
          es.append(0)
          emptySeriesList.put(SeriesInstanceUID, es)
          logger.warn("New Empty series:\n" + es)
        }
      }
    }

    sliceList.isEmpty
  }

  def getSize: Int = emptySeriesList.synchronized(emptySeriesList.size)

  def toText: String = {
    s"Number of empty series: $getSize \n" + emptySeriesList.mkString("\n")
  }

  def main(args: Array[String]): Unit = {

    logger.info("Starting")

    // val uid = "1.2.246.352.62.2.5096116441414362373.3768427654831361972" // real
    val uid = "12345" // fake

    if (ClientConfig.validate) {
      logger.info("Validated configuration")
      println(seriesIsEmpty(uid, "NoPat", "NoModality"))
      Thread.sleep(100)
      println(seriesIsEmpty(uid, "NoPat", "NoModality"))
      Thread.sleep(100)
      println(seriesIsEmpty(uid, "NoPat", "NoModality"))
      Thread.sleep(100)
      println(seriesIsEmpty(uid, "NoPat", "NoModality"))
      val list = DicomFind.getSliceUIDsInSeries(uid, "NoPat", "NoModality")
      println(list.mkString("  |  "))
    }
  }
}
