package org.babysitter

import java.util._

trait BabysitterTools {

  def validateStartTime(start: Int): Int = {
    if(1700 < start) {
      start
    } else {
      1700
    }
  }

  def validateEndTime(end: Int): Int = {
    if(400 > end) {
      end
    } else {
      400
    }
  }

  def roundToNearestHour(time: Int): Int = {
    val minutes = time % 100
    if(minutes <= 30){
      time - minutes
    } else {
      (time - minutes) + 100
    }
  }

  def payFromStartToBedtime(start: Int, end: Int): Int = {
    val startToBedtimePay: Int = 12
    (start, end) match {
      case (startTime, _) if (startTime >= 2100) => 0
      case (startTime, endTime) if (startTime >= 1700 && endTime >= 1700) => {
        val hoursMilitaryTime = startTime - endTime
        val numberOfHours = hoursMilitaryTime / 100
        val payToBedtime = numberOfHours * startToBedtimePay
        Math.abs(payToBedtime)
      }

      case (startTime, endTime) if (startTime >= 1700 && endTime <= 400) => {
        val hoursMilitaryTime = startTime - (endTime + 1700)
        val numberOfHours = hoursMilitaryTime / 100
        val payToBedtime = numberOfHours * startToBedtimePay
        Math.abs(payToBedtime)
      }
    }
  }

  def payFromBedtimeToMidnight(start: Int, end: Int): Int = {
    val bedtimeToMidnightPay: Int = 8
    (start, end) match {
      case (startTime, endTime) if (startTime <= 2100 && (endTime >= 2100 || endTime <= 400)) => {
        val hoursMilitaryTime = startTime - 2400
        val numberOfHours = hoursMilitaryTime / 100
        val payToMidnight = numberOfHours * bedtimeToMidnightPay
        Math.abs(payToMidnight)
      }
      case (startTime, endTime) if (startTime >= 2100 && endTime <= 2400) => {
        val hoursMilitaryTime = startTime - endTime
        val numberOfHours = hoursMilitaryTime / 100
        val payToMidnight = numberOfHours * bedtimeToMidnightPay
        Math.abs(payToMidnight)
      }
      case (startTime, _) if (startTime <= 2400 || (100 <= startTime && startTime <= 400)) => 0
    }
  }

  def payFromMidnightToEnd(start: Int, end: Int): Int = {
    val midnightToEndPay: Int = 16
      (start, end) match {
        case (startTime, endTime) if (startTime >= 100 && startTime <= 400 && endTime <= 400) => {
          val diff = Math.abs(400 - endTime)
          val hoursMilitaryTime = endTime - diff
          val numberOfHours = hoursMilitaryTime / 100
          val payToEnd = numberOfHours * midnightToEndPay
          Math.abs(payToEnd)
        }
        case (startTime, endTime) if ((startTime == 2400 || startTime <= 400) && endTime >= 400) => {
          val diff = Math.abs(400 - endTime)
          val hoursMilitaryTime = endTime - diff
          val numberOfHours = hoursMilitaryTime / 100
          val payToEnd = numberOfHours * midnightToEndPay
          Math.abs(payToEnd)
        }
      case (startTime, endTime) if (startTime >= 400 && startTime <= 2400 && endTime <= 400) => {
        val numberOfHours = endTime / 100
        val payToEnd = numberOfHours * midnightToEndPay
        Math.abs(payToEnd)
      }
    }
  }
}

object BabysitterBoot extends BabysitterTools with App {

  println("What was your start time? Please enter UTC military time.")
  println("Ex: 5:00 p.m. '1700'")
  val inputStart: String = scala.io.StdIn.readLine().toString
  val validStart = validateStartTime(inputStart.toInt)
  println("validStart: " + validStart)

  println("What was your end time? Please enter UTC military time.")
  println("Ex: 4:00 a.m. '400'")
  val inputEnd: String = scala.io.StdIn.readLine().toString
  val validEnd = validateEndTime(inputEnd.toInt)
  println("validEnd: " + validEnd)
}
