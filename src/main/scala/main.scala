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
    (start, end) match {
      case (startTime, _)
        if (timeIsAfterBedtime(startTime)) =>
          0
      case (startTime, endTime)
        if (startTimeAfterStartCutoff(startTime) && endTimeAfterStartCutoff(endTime)) =>
          val workedHours = startTime - endTime
          calculatePay(startToBedtimePay, workedHours)
      case (startTime, endTime)
        if (startTimeAfterStartCutoff(startTime) && timeIsAfterEndCutoff(endTime)) =>
          val workedHours = startTime - (endTime + START_CUTOFF)
          calculatePay(startToBedtimePay, workedHours)
      case (startTime, endTime)
        if (startTimeAfterStartCutoff(startTime) && endTimeAfterStartCutoff(endTime)) =>
          val workedHours = startTime - endTime
          calculatePay(startToBedtimePay, workedHours)
      case (startTime, endTime)
        if (startTimeAfterStartCutoff(startTime) && timeIsBeforeEndCutoff(endTime)) =>
          val workedHours = BEDTIME - startTime
          println(workedHours)
          calculatePay(startToBedtimePay, workedHours)
    }
  }

  def payFromBedtimeToMidnight(start: Int, end: Int): Int = {
    val bedtimeToMidnightPay: Int = 8
    (start, end) match {
      case (startTime, endTime)
        if (startTime < BEDTIME && (timeIsBeforeEndCutoff(endTime) || endTime == MIDNIGHT)) =>
          calculatePay(bedtimeToMidnightPay, 300)
      case (startTime, endTime)
        if (startTimeIsBeforeBedtime(startTime) && (timeIsAfterBedtime(endTime) || timeIsBeforeEndCutoff(endTime))) =>
          val hoursWorked = startTime - MIDNIGHT
          calculatePay(bedtimeToMidnightPay, hoursWorked)
      case (startTime, endTime)
        if (timeIsAfterBedtime(startTime) && timeIsBeforeMidnight(endTime)) =>
          val hoursWorked = startTime - endTime
          calculatePay(bedtimeToMidnightPay, hoursWorked)
      case (startTime, _)
        if (timeIsBeforeMidnight(startTime) || (100 <= startTime && timeIsBeforeEndCutoff(startTime))) =>
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
