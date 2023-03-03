package org.aqaclient

import edu.umro.ScalaUtil.Logging
import edu.umro.ScalaUtil.Trace

import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Node

/**
  * Support variable length polling.  The approach is to poll recently active
  * patients more frequently than those that have not had activity in a longer
  * time.  This is an optimization to reduce the amount of polling.
  *
  * This is particularly applicable to test patients that have fallen out of
  * use but are still on the list to be polled.
  *
  * @param poll_ms Polling frequency in ms.
  * @param age_ms Files must be at least this recent to qualify for polling.
  */
case class PollInterval(poll_ms: Long, age_ms: Long) extends Logging {

  /** The last time a poll was performed for this interval.  (ms since Jan 1 1970) */
  private var pollTime_ms: Long = 0

  /**
    * Determine if a patient with DICOM of the given age should be polled.
    *
    * @param date Time of the latest DICOM file.
    * @return True if polling should be performed.
    */
  private def shouldBePolled(date: Date): Boolean =
    this.synchronized {
      val now = System.currentTimeMillis()
      val dateAge_ms = now - date.getTime
      val timeSincePoll_ms = now - pollTime_ms
      val poll = (dateAge_ms <= age_ms) && (timeSincePoll_ms >= poll_ms)
      poll
    }

  private def timeRemaining: Long =
    this.synchronized {
      val timeSincePoll_ms = System.currentTimeMillis() - pollTime_ms
      val remaining = poll_ms - timeSincePoll_ms
      remaining
    }

  /**
    * Determine if this polling interval is out of date.
    *
    */
  private def isExpired(update: Boolean = false): Boolean =
    this.synchronized {
      val timeSincePoll_ms = System.currentTimeMillis() - pollTime_ms
      val expired = timeSincePoll_ms >= poll_ms

      if (expired && update)
        pollTime_ms = System.currentTimeMillis()

      expired
    }

  override def toString: String = {
    "poll (sec): " + (poll_ms / 1000.0).formatted("%6.1f") +
      "      age (day): " + (age_ms.toDouble / PollInterval.msInDay).formatted("%6.1f") +
      "      timeRemaining (sec): " + (timeRemaining / 1000.0).formatted("%6.1f") +
      "      isExpired: " + isExpired().toString.format("%5s")
  }
}

object PollInterval {

  private val msInDay = 24 * 60 * 60 * 1000
  private def construct(node: Node): PollInterval = {
    val poll_sec = (node \ "@poll_sec").text.toDouble
    val age_day = (node \ "@age_day").text.toDouble

    PollInterval(poll_ms = (poll_sec * 1000).round, age_ms = (age_day * msInDay).round)
  }

  /** List of all polling intervals, sorted by age. */
  private val list: scala.collection.mutable.ArrayBuffer[PollInterval] = scala.collection.mutable.ArrayBuffer()

  /**
    * Determine the given date indicates that activity has been sufficiently recent so as to warrant polling.
    * @param date Date of last activity (latest file).
    * @return True if polling should be done.
    */
  def shouldBePolled(date: Date): Boolean =
    this.synchronized {
      list.exists(pi => pi.shouldBePolled(date)) || list.last.isExpired()
    }

  /**
    * Get the maximum file age of all the polling intervals.
    * @return
    */
  def maxAge(): Long = this.synchronized { list.maxBy(_.age_ms).age_ms }

  private def updatePollTimeIfExpired(): Unit =
    this.synchronized {
      list.foreach(_.isExpired(update = true))
    }

  /**
    * Given the configuration node <code>PollIntervalList</code>, construct the list of poll intervals.
    *
    * @param pollIntervalList Config node.
    * @return Text description.
    */
  def init(pollIntervalList: Node): String = {
    val seq = (pollIntervalList \ "PollInterval").map(construct).sortBy(_.age_ms)
    this.synchronized {
      seq.foreach(pi => list.append(pi))
      "\n        " + list.mkString("\n        ")
    }

  }

  def main(args: Array[String]): Unit = {
    Trace.trace()
    ClientConfig.validate
    Trace.trace("--------------------------------------------------------------------------")
    Trace.trace("--------------------------------------------------------------------------")
    Trace.trace("--------------------------------------------------------------------------")
    Trace.trace("--------------------------------------------------------------------------")
    Trace.trace("--------------------------------------------------------------------------")

    // list of ages in days to test
    val dateList = Seq(0.2, 1.2, 3.8, 9.3, 66.0, 100.0, 194.0, 400.0, 1000.0)

    val dateFormat = new SimpleDateFormat("yyyy MM dd  HH:mm")

    def checkAll(): Unit = {

      def check(age_day: Double): Unit = {
        val age_ms = (age_day * msInDay).round
        val date = new Date(System.currentTimeMillis() - age_ms)
        val poll = shouldBePolled(date)
        println("Age in days: " + age_day.formatted("%8.1f") + " == " + dateFormat.format(date) + "  expired: " + poll.toString.format("%5s"))
      }

      Trace.trace("========================")
      list.foreach(println)
      println
      dateList.foreach(check)
      Trace.trace("========================")
    }

    Trace.trace("------------ init --------------------------------------------------------")
    checkAll()

    Trace.trace("--------------------------------------------------------------------------")
    Trace.trace("------------ after init --------------------------------------------------")

    updatePollTimeIfExpired()

    Trace.trace("--------------------------------------------------------------------------")
    Trace.trace("------------ after update -----------------------------------------------")

    checkAll()
    Trace.trace("--------------------------------------------------------------------------")

    Trace.trace("Sleeping ...")
    Thread.sleep(16 * 1000)

    Trace.trace("--------------------------------------------------------------------------")
    Trace.trace("------------ after 16 wait -----------------------------------------------")
    checkAll()
    Trace.trace("--------------------------------------------------------------------------")

    Thread.sleep(46 * 1000)
    Trace.trace("------------ after 46 wait -----------------------------------------------")
    checkAll()

    Thread.sleep(50 * 1000)
    Trace.trace("------------ after 66 wait -----------------------------------------------")
    checkAll()

    System.exit(99)
  }
}
