package org.aqaclient

/**
 * Enumerate different types of procedures.  Note that procedures are different from
 * RTPLAN types, because a single type of RTPLAN may be used for different procedures.
 */
object ProcedureEnum extends Enumeration {
  val Phase2 = Value
  val DailyQACT = Value
  val DailyQARTIMAGE = Value
  val LOC = Value
  val LOCBaseline = Value

  /**
   * Convert text to ProcedureEnum.  Throw exception on failure to match.
   */
  def toProcedureEnum(text: String) = {
    values.filter(v => v.toString.equalsIgnoreCase(text)).toSeq.head
  }

  def procedureNameIsPhase2(text: String) = {
    text.toLowerCase.contains("phase2")
  }

  def procedureNameIsDailyQACT(text: String) = {
    text.toLowerCase.contains("bb") && text.toLowerCase.contains("cbct")
  }

  def procedureNameIsDailyQARTIMAGE(text: String) = {
    text.toLowerCase.contains("bb") && text.toLowerCase.contains("epid")
  }

  def procedureNameIsLOC(text: String) = {
    (text.toLowerCase.contains("loc") || text.toLowerCase.contains("leaf offset")) && (!text.toLowerCase.contains("base"))
  }

  def procedureNameIsLOCBaseline(text: String) = {
    text.toLowerCase.contains("base")
  }

  def procNameToProcedureEnum(procName: String): Option[ProcedureEnum.Value] = {
    procName match {
      case _ if (ProcedureEnum.procedureNameIsDailyQACT(procName)) => Some(ProcedureEnum.DailyQACT)
      case _ if (ProcedureEnum.procedureNameIsDailyQARTIMAGE(procName)) => Some(ProcedureEnum.DailyQARTIMAGE)
      case _ if (ProcedureEnum.procedureNameIsPhase2(procName)) => Some(ProcedureEnum.Phase2)
      case _ if (ProcedureEnum.procedureNameIsLOC(procName)) => Some(ProcedureEnum.LOC)
      case _ if (ProcedureEnum.procedureNameIsLOCBaseline(procName)) => Some(ProcedureEnum.LOCBaseline)
      case _ => None
    }
  }

}