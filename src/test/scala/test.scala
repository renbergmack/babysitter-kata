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
}
