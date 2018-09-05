package org.babysitter

import org.scalatest._
import Matchers._

class BabysitterTest extends FlatSpec with BabysitterTools {

  "setStartTime" should "return a 1700 when an earlier start time is declared" in {
    val earlierStart: Int = 1600
    setStartTime(earlierStart) should be (1700)
  }

  it should "return a the time when a time later than 1700 is declared" in {
    val earlierStart: Int = 1800
    setStartTime(earlierStart) should be (1800)
  }

  "setEndTime" should "return 0400 when a time later than 400 is declared" in {
    val lateEnd: Int = 500
    setEndTime(lateEnd) should be (400)
  }

  it should "return the end time when a time earlier than 400 is declared" in {
    val earlyEnd: Int = 300
    setEndTime(earlyEnd) should be (300)
  }

  "roundToNearestHour" should "return closest hourly time when minutes less than 30" in {
    val earlyStart: Int = 1725
    roundToNearestHour(earlyStart) should be (1700)
  }

  it should "return closest hourly time when minutes more than 30" in {
    val lateStart: Int = 1735
    roundToNearestHour(lateStart) should be (1800)
  }

  it should "return earlier hourly time when minutes equal to 30" in {
    val evenStart: Int = 1730
    roundToNearestHour(evenStart) should be (1700)
  }

  "payFromStartToBedtime" should "return no pay if start is past bedtime(2100)" in {
    val start = 2100
    val end = 400
    payFromStartToBedtime(start, end) should be (0)
  }

  it should "return 12 dollar pay if start is before bedtime" in {
    val start = 1700
    val end = 2100
    payFromStartToBedtime(start, end) should be (48)
  }

  it should "return 12 dollar pay if end is after bedtime" in {
    val start = 1700
    val end = 400
    payFromStartToBedtime(start, end) should be (48)
  }

  it should "return 12 dollar pay if end is before bedtime" in {
    val start = 1700
    val end = 2000
    payFromStartToBedtime(start, end) should be (36)
  }

  "payFromBedtimeToMidnight" should "return no pay if start time is past midnight(2400)" in {
    val start = 2400
    val end = 2400
    payFromBedtimeToMidnight(start, end) should be (0)
  }

  it should "return 8 dollar pay if start is at bedtime" in {
    val start = 2100
    val end = 2400
    payFromBedtimeToMidnight(start, end) should be (24)
  }

  it should "return 8 dollar pay if end is after midnight" in {
    val start = 2100
    val end = 400
    payFromBedtimeToMidnight(start, end) should be (24)
  }

  it should "return 8 dollar pay if start and end is between bedtime and midnight" in {
    val start = 2200
    val end = 2300
    payFromBedtimeToMidnight(start, end) should be (8)
  }

  "payFromMidnightToEnd" should "return pay if start time is past end(400)" in {
    val start = 500
    val end = 400
    payFromMidnightToEnd(start, end) should be (64)
  }

  it should "return 16 dollar pay if start is at midnight" in {
    val start = 2400
    val end = 400
    payFromMidnightToEnd(start, end) should be (64)
  }

  it should "return 16 dollar pay if end is after midnight" in {
    val start = 2400
    val end = 500
    payFromMidnightToEnd(start, end) should be (64)
  }

  it should "return 16 dollar pay if start and end is between midnight and end" in {
    val start = 100
    val end = 300
    payFromMidnightToEnd(start, end) should be (32)
  }
}
