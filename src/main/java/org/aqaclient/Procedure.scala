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

import edu.umro.ScalaUtil.Logging

import scala.xml.Node

object Procedure {
  /** Maintain a cache of procedures defined on the server. */
  private val procedureCache = scala.collection.mutable.Set[Procedure]()

  /**
    * Get the list of procedures.
    *
    * @return List of procedures.
    */
  def fetchList(): Seq[Procedure] =
    procedureCache.synchronized {
      procedureCache.toSeq
    }

  /**
    * Replace the cache contents with the given list.
    *
    * @param newList New list of procedures.
    */
  def setList(newList: Seq[Procedure]): Unit =
    procedureCache.synchronized {
      procedureCache.clear()
      newList.foreach(p => procedureCache.add(p))
    }
}

class Procedure(val node: Node) extends Logging {
  val Version: String = textOf(tag = "Version")
  val Name: String = textOf(tag = "Name")
  val URL: String = {
    val fullUrl = ClientConfig.AQAURL + textOf(tag = "URL") + "?Run=Run&AutoUpload=true"
    fullUrl
  }

  /**
    * Get the text of an attribute or element by the same name.
    * @param tag Tag name.
    * @return Text value.
    */
  private def textOf(tag: String) = ((node \ tag) ++ (node \ ("@" + tag))).head.text.trim

  final val isBBbyCBCT = Name.toLowerCase.contains("bb") && Name.toLowerCase.contains("cbct")
  final val isBBbyEPID = Name.toLowerCase.contains("bb") && Name.toLowerCase.contains("epid")
  final val isPhase2 = Name.toLowerCase.matches(".*phase *2.*")
  final val isLOC = (Name.toLowerCase.contains("loc") || Name.toLowerCase.contains("leaf offset")) && (!Name.toLowerCase.contains("base"))
  final val isLOCBaseline = Name.toLowerCase.contains("loc") && Name.toLowerCase.contains("base")
  final val isMachineLog = Name.toLowerCase.contains("mach") && Name.toLowerCase.contains("log")

  override def toString: String = {
    Name + " : " + Version
  }

  /**
    * Return the list of modalities for this procedure.
    *
    * Note that by necessity this is 'hard coded' in that the list is derived
    * from knowing the list of modalities that each procedure uses.
    */
  val modalityList: Seq[String] = {
    if (isBBbyCBCT)
      Seq("CT", "REG", "RTPLAN")
    else
      Seq("RTIMAGE", "RTPLAN")
  }

  /*
  logger.info(
    "Constructed procedure " + toString +
      "    isBBbyCBCT: " + isBBbyCBCT.toString.head +
      "    isBBbyEPID: " + isBBbyEPID.toString.head +
      "    isPhase2: " + isPhase2.toString.head +
      "    isLOC: " + isLOC.toString.head +
      "    isLOCBaseline: " + isLOCBaseline.toString.head +
      "    URL: " + URL
  )
   */
}
