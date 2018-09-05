package org.babysitter

import org.scalatest._
import Matchers._

class BabysitterTest extends FlatSpec with BabysitterTools {

  "validateStartTime" should "return a 1700 when an earlier start time is declared" in {
    val earlierStart: Int = 1600
    validateStartTime(earlierStart) should be (1700)
  }

  it should "return a the time when a time later than 1700 is declared" in {
    val earlierStart: Int = 1800
    validateStartTime(earlierStart) should be (1800)
  }

  "validateEndTime" should "return 0400 when a time later than 0400 is declared" in {
    val lateEnd: Int = 500
    validateEndTime(lateEnd) should be (400)
  }

  it should "return the end time when a time earlier than 0400 is declared" in {
    val earlyEnd: Int = 300
    validateEndTime(earlyEnd) should be (300)
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

  it should "return 12 dollar pay if start is before bedtime(2100)" in {
    val start = 1700
    val end = 2100
    payFromStartToBedtime(start, end) should be (48)
  }

  it should "return 12 dollar pay if end is greater than(2100)" in {
    val start = 1700
    val end = 400
    payFromStartToBedtime(start, end) should be (48)
  }

  it should "return 12 dollar pay if end is less than(2100)" in {
    val start = 1700
    val end = 2000
    payFromStartToBedtime(start, end) should be (36)
  }

  // "payFromBedtimeToMidnight" should "return no pay if start time is past bedtime(2400)" in {
  //   val start = 2100
  //   val end = 400
  //   payFromStartToBedtime(start, end) should be (0)
  // }
  //
  // it should "return 8 dollar pay if start is after bedtime(2100)" in {
  //   val start = 1700
  //   val end = 400
  //   payFromStartToBedtime(start, end) should be (48)
  // }
}
