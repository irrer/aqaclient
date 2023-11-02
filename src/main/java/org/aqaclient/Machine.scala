/*
 * Copyright 2023 Regents of the University of Michigan
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

import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.Logging

import java.io.File
import scala.xml.Node
import scala.xml.XML

case class Machine(
    AnonymousId: String,
    Id: String,
    Collimator: String,
    EPID: String,
    SerialNumber: Option[String],
    ImagingBeam2_5_mv: Boolean,
    OnboardImager: Boolean,
    Table6DOF: Boolean,
    RespiratoryManagement: Boolean,
    DeveloperMode: Boolean,
    Active: Boolean,
    TpsID: Option[String],
    Notes: String
) extends Logging {}

/**
  * Support getting the list of treatment machines from the server for this institution.  Having this information allows the
  * client to make more intelligent decisions as to which data sets to upload.
  *
  * @author Jim Irrer irrer@med.umich.edu
  */
object Machine extends Logging {

  /** If the fetched list of machines is older than this, then get a new list to ensure it is up to date. */
  private val maxCacheAge_min = 60.0

  // value in ms for convenience.
  private val maxCacheAge_ms = (maxCacheAge_min * 60 * 1000).toLong

  /** Cached copy of machine list. */
  private var cache: Option[Seq[Machine]] = None

  /** Time in ms when cache was last updated. */
  private var cacheTime_ms: Long = 0

  /**
    * Determine if cache is up to date.
    * @return True if up to date.
    */
  private def cacheIsUpToDate = cacheTime_ms > (System.currentTimeMillis() - maxCacheAge_ms)

  /**
    * Given an element, construct a treatment machine from it.
    *
    * On failure return None
    *
    * @param node Source XML
    * @return machine or None
    */
  private def xmlToMachine(node: Node): Option[Machine] = {

    def toBool(tag: String): Boolean = (node \ tag).head.text.toLowerCase().trim.toBoolean

    def toText(tag: String): String = (node \ tag).head.text
    def toOptText(tag: String): Option[String] = {
      try {
        val nodeSeq = node \ tag
        if (nodeSeq.isEmpty)
          None
        else
          Some(nodeSeq.head.text)

      } catch {
        case _: Throwable => None
      }
    }

    try {

      val machine = Machine(
        AnonymousId = toText("AnonymousId"),
        Id = toText("Id"),
        Collimator = toText("Collimator"),
        EPID = toText("EPID"),
        SerialNumber = toOptText("SerialNumber"),
        ImagingBeam2_5_mv = toBool("ImagingBeam2_5_mv"),
        OnboardImager = toBool("OnboardImager"),
        Table6DOF = toBool("Table6DOF"),
        RespiratoryManagement = toBool("RespiratoryManagement"),
        DeveloperMode = toBool("DeveloperMode"),
        Active = toBool("Active"),
        TpsID = toOptText("TpsID"),
        Notes = toText("Notes")
      )

      Some(machine)
    } catch {
      case t: Throwable =>
        logger.warn(s"Could not convert XML to Machine ${fmtEx(t)}\n${node.toString}")
        None
    }

  }

  /**
    * If the latest version from the server has changed, then update the local file.  Note
    * that this file is never read, it is only saved as an aid for diagnosing problems.
    *
    * @param text XML text describing treatment machines.
    */
  private def saveMachineXmlIfNew(text: String): Unit = {
    val file = new File(ClientConfig.DataDir, "Machine.xml")

    def write(): Unit = {
      FileUtil.writeFile(file, text)
      logger.info(s"Wrote new version of file ${file.getAbsolutePath}")
    }

    FileUtil.readTextFile(file) match {
      case Left(_) => write()
      case Right(previousText) =>
        if (!text.equals(previousText))
          write()
    }
  }

  private def updateFromServer(): Option[Seq[Machine]] = {
    logger.info("Getting latest list of treatment machines from the server...")
    val url = ClientConfig.AQAURL + "/admin/MachineXml"
    HttpUtil.httpsGet(url) match {
      case Some(text) =>
        try {
          val list = (XML.loadString(text) \ "Machine").map(xmlToMachine)
          logger.info(s"Got list of ${list.size} treatment machines from the server...")
          saveMachineXmlIfNew(text)
          Some(list.flatten.sortBy(_.Id))
        } catch {
          case _: Throwable =>
            logger.warn(s"Was not able to get list of treatment machines from the server.")
            None
        }
      case None =>
        logger.warn(s"Failed to get list of treatment machines from the server.")
        None
    }
  }

  private def updateCache(): Unit = {
    val latest = updateFromServer()

    cacheTime_ms.synchronized {
      cache = latest
      cacheTime_ms = System.currentTimeMillis()
    }
  }

  /**
    * Get the list of treatment machines defined on the server.
    *
    * @return List of treatment machines.
    */
  def getMachineList: Option[Seq[Machine]] = {

    if (!cacheIsUpToDate)
      updateCache()

    cache
  }

  /**
    * For testing only.
    *
    * @param args Not used.
    */
  def main(args: Array[String]): Unit = {

    ClientConfig.validate

    println("Starting ---------------------------------------")

    def show(attempt: Int): Unit = {
      println(s"attempt: $attempt")
      val a = getMachineList
      if (a.isDefined)
        println(s"Number of machines: ${a.get.size}")
      else
        println("Could not get list of machines.")
    }

    show(1)
    show(2)
    Thread.sleep(1000)
    show(3)

  }
}
