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
}