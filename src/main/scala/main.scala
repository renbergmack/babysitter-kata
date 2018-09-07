package org.babysitter

import java.util._

trait BabysitterTools {

  val START_CUTOFF = 1700
  val END_CUTOFF = 400
  val BEDTIME = 2100
  val MIDNIGHT = 2400

  def setStartTime(babysitterStart: Int): Int = {
    if(START_CUTOFF < babysitterStart) {
      babysitterStart
    } else {
      START_CUTOFF
    }
  }

  def setEndTime(babysitterEnd: Int): Int = {
    if(END_CUTOFF > babysitterEnd) {
      babysitterEnd
    } else {
      END_CUTOFF
    }
  }

  def roundToNearestHour(hourlyMilitaryTime: Int): Int = {
    val minutes = hourlyMilitaryTime % 100
    if(minutes <= 30){
      val previousHour = hourlyMilitaryTime - minutes
      previousHour
    } else {
      val previousHour = hourlyMilitaryTime - minutes
      val nextHour = previousHour + 100
      nextHour
    }
  }

  def calculatePay(payRate: Int, hoursMilitaryTime: Int): Int = {
    val numberOfHours = hoursMilitaryTime / 100
    val payToBedtime = numberOfHours * payRate
    Math.abs(payToBedtime)
  }

  def hoursWorkedBetweenMidnightAndEnd(endTime: Int): Int = {
    val hoursBeforeEnd = Math.abs(400 - endTime)
    val hoursWorked = endTime - hoursBeforeEnd
    hoursWorked
  }

  def startTimeAfterStartCutoff(startTime: Int): Boolean = {
    startTime >= START_CUTOFF
  }

  def endTimeAfterStartCutoff(startTime: Int): Boolean = {
    startTime >= START_CUTOFF
  }

  def startTimeIsBeforeBedtime(startTime: Int): Boolean = {
    startTime <= BEDTIME
  }

  def timeIsAfterBedtime(time: Int): Boolean = {
    time >= BEDTIME
  }

  def timeIsBeforeEndCutoff(time: Int): Boolean = {
    time <= END_CUTOFF
  }

  def timeIsAfterEndCutoff(time: Int): Boolean = {
    time >= END_CUTOFF
  }

  def timeIsBeforeMidnight(time: Int): Boolean = {
    time <= MIDNIGHT
  }

  def timeBetweenMidnightAndEndCutoff(time: Int): Boolean = {
    time == MIDNIGHT || timeIsBeforeEndCutoff(time)
  }

  def payFromStartToBedtime(start: Int, end: Int): Int = {
    val startToBedtimePay: Int = 12
    if (timeIsAfterBedtime(start)) {
      0
    } else if (startTimeAfterStartCutoff(start) && endTimeAfterStartCutoff(end)) {
      val workedHours = start - end
      calculatePay(startToBedtimePay, workedHours)
    } else if (startTimeAfterStartCutoff(start) && timeIsAfterEndCutoff(end)) {
      val workedHours = start - (end + START_CUTOFF)
      calculatePay(startToBedtimePay, workedHours)
    } else if (startTimeAfterStartCutoff(start) && timeIsBeforeEndCutoff(end)) {
      val workedHours = BEDTIME - start
      calculatePay(startToBedtimePay, workedHours)
    } else {
      println(s"Pay from start to bedtime could not be calculated. Start: ${start}, End: ${end}")
      0
    }
  }

  def payFromBedtimeToMidnight(start: Int, end: Int): Int = {
    val bedtimeToMidnightPay: Int = 8
    if (start < BEDTIME && (timeIsBeforeEndCutoff(end) || end == MIDNIGHT)) {
      calculatePay(bedtimeToMidnightPay, 300)
    } else if (startTimeIsBeforeBedtime(start) && (timeIsAfterBedtime(end) || timeIsBeforeEndCutoff(end))) {
      val hoursWorked = start - MIDNIGHT
      calculatePay(bedtimeToMidnightPay, hoursWorked)
    } else if (timeIsAfterBedtime(start) && timeIsBeforeMidnight(end)) {
      val hoursWorked = start - end
      calculatePay(bedtimeToMidnightPay, hoursWorked)
    } else if (timeIsBeforeMidnight(start) || (100 <= start && timeIsBeforeEndCutoff(start))) {
      0
    } else {
      println(s"Pay from bedtime to midnight could not be calculated. Start: ${start}, End: ${end}")
      0
    }
  }

  def payFromMidnightToEnd(start: Int, end: Int): Int = {
    val midnightToEndPay: Int = 16
      (start, end) match {
        case (startTime, endTime)
          if (startTime >= 100 && timeIsBeforeEndCutoff(startTime) && timeIsBeforeEndCutoff(endTime)) =>
            val hoursWorked = hoursWorkedBetweenMidnightAndEnd(endTime)
            calculatePay(midnightToEndPay, hoursWorked)
        case (startTime, endTime)
          if (timeBetweenMidnightAndEndCutoff(startTime) && timeIsAfterEndCutoff(endTime)) =>
            val hoursWorked = hoursWorkedBetweenMidnightAndEnd(endTime)
            calculatePay(midnightToEndPay, hoursWorked)
        case (startTime, endTime)
          if (timeIsAfterEndCutoff(startTime) && timeIsBeforeMidnight(startTime) && timeIsBeforeEndCutoff(endTime)) =>
            calculatePay(midnightToEndPay, endTime)
    }
  }
}

object BabysitterBoot extends BabysitterTools with App {

  println("What was your start time? Please enter UTC military time.")
  println("Ex: 5:00 p.m. '1700'")
  val inputStart: String = scala.io.StdIn.readLine().toString
  val startTime = setStartTime(inputStart.toInt)
  println("Start time entered: " + startTime)

  println("What was your end time? Please enter UTC military time.")
  println("Ex: 4:00 a.m. '400'")
  val inputEnd: String = scala.io.StdIn.readLine().toString
  val endTime: Int = setEndTime(inputEnd.toInt)
  println("End time entered: " + endTime)

  val payToBedtime: Int = payFromStartToBedtime(startTime, endTime)
  println("payToBedtime: " + payToBedtime)
  val payToMidnight: Int = payFromBedtimeToMidnight(startTime, endTime)
  println("payToMidnight: " + payToMidnight)
  val payToEnd: Int = payFromMidnightToEnd(startTime, endTime)
  println("payToEnd: " + payToEnd)
  val totalPay: Int = payToBedtime + payToMidnight + payToEnd

  println("Your total pay is: " + totalPay)
}
