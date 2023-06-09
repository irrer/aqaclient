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
    * Given the name of the procedure, find the procedure.
    *
    * @param name Name of procedure.
    *
    * @return The Procedure, if found.
    */
  def procedureByName(name: String): Option[Procedure] = {

    def sameProc(procedure: Procedure): Boolean = {
      isList.exists(is => {
        is(name) && is(procedure.Name)
      })
    }

    val proc = fetchList().find(p => sameProc(p))
    proc
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

  /**
    * Given a name, return true if it matches a given procedure.
    *
    * Note that procedures should have a standard name.  This is a design flaw and might be fixed in the future.
    *
    * @param name Text indicating which procedure.
    * @return
    */
  private def isBBbyCBCT(name: String): Boolean = name.toLowerCase.contains("bb") && name.toLowerCase.contains("cbct")
  private def isBBbyEPID(name: String): Boolean = name.toLowerCase.contains("bb") && name.toLowerCase.contains("epid")
  private def isPhase2(name: String): Boolean = name.toLowerCase.matches(".*phase *2.*")
  private def isLOC(name: String): Boolean = (name.toLowerCase.contains("loc") || name.toLowerCase.contains("leaf offset")) && (!name.toLowerCase.contains("base"))
  private def isLOCBaseline(name: String): Boolean = name.toLowerCase.contains("loc") && name.toLowerCase.contains("base")
  private def isMachineLog(name: String): Boolean = name.toLowerCase.contains("mach") && name.toLowerCase.contains("log")
  private def isGapSkew(name: String): Boolean = name.toLowerCase.contains("gap") && name.toLowerCase.contains("skew")
  private def isWinstonLutz(name: String): Boolean = name.toLowerCase.contains("winston") && name.toLowerCase.contains("lutz")

  private val isList = Seq(
    isBBbyCBCT _,
    isBBbyEPID _,
    isPhase2 _,
    isLOC _,
    isLOCBaseline _,
    isMachineLog _,
    isGapSkew _,
    isWinstonLutz _
  )

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

  final val isBBbyCBCT = Procedure.isBBbyCBCT(Name)
  final val isBBbyEPID = Procedure.isBBbyEPID(Name)
  // final val isPhase2 = Procedure.isPhase2(Name)
  // final val isLOC = Procedure.isLOC(Name)
  // final val isLOCBaseline = Procedure.isLOCBaseline(Name)
  final val isMachineLog = Procedure.isMachineLog(Name)
  final val isWinstonLutz = Procedure.isWinstonLutz(Name)

  override def toString: String = {
    Name + " : " + Version
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
