package org.aqaclient

/**
 * Repository for series that are problematic to fetch via DICOM.
 *
 * There are some series who's UIDs are listed via C-FIND for a
 * patient, but can not be fetched via C-MOVE.  It is useful to
 * keep track of them to avoid retrying to fetch them ad infinitum.
 */
object FailedSeries {
  private val failedSeries = scala.collection.mutable.HashSet[String]()

  def put(SeriesInstanceUID: String): Unit = failedSeries.synchronized(failedSeries += SeriesInstanceUID)

  def contains(SeriesInstanceUID: String): Boolean = failedSeries.synchronized(failedSeries.contains(SeriesInstanceUID))
}