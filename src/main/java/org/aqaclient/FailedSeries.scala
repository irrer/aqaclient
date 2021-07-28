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

/**
 * Repository for series that are problematic to fetch via DICOM.
 *
 * There are some series who's UIDs are listed via C-FIND for a
 * patient, but can not be fetched via C-MOVE.  It is useful to
 * keep track of them to avoid retrying to fetch them ad infinitum.
 */
object FailedSeries {
  private val failedSeries = scala.collection.mutable.HashSet[String]()

  def put(SeriesInstanceUID: String): Unit =
    failedSeries.synchronized(failedSeries += SeriesInstanceUID)

  def contains(SeriesInstanceUID: String): Boolean =
    failedSeries.synchronized(failedSeries.contains(SeriesInstanceUID))
}