package org.babysitter

trait BabysitterTools {

  val START_CUTOFF = 1700
  val BEDTIME = 2100
  val MIDNIGHT = 2400
  val END_CUTOFF = 400

  def setStartTime(babysitterStart: Int): Int = {
    if (START_CUTOFF < babysitterStart) {
      babysitterStart
    } else {
      START_CUTOFF
    }
  }

  def setEndTime(babysitterEnd: Int): Int = {
    if (END_CUTOFF > babysitterEnd) {
      babysitterEnd
    } else {
      END_CUTOFF
    }
  }

  def roundToNearestHour(hourlyMilitaryTime: Int): Int = {
    val minutes = hourlyMilitaryTime % 100
    val previousHour = hourlyMilitaryTime - minutes
    if (minutes <= 30) {
      previousHour
    } else {
      val nextHour = previousHour + 100
      nextHour
    }
  }

  def calculatePay(payRate: Int, hoursMilitaryTime: Int): Int = {
    val hours = hoursMilitaryTime / 100
    val payToBedtime = hours * payRate
    Math.abs(payToBedtime)
  }

  def hoursWorkedBetweenMidnightAndEnd(end: Int): Int = {
    val hoursBeforeEnd = Math.abs(END_CUTOFF - end)
    val hoursWorked = end - hoursBeforeEnd
    hoursWorked
  }

  def timeIsAfterMidnight(start: Int): Boolean = {
    start >= 100
  }

  def timeIsAfterStartCutoff(start: Int): Boolean = {
    start >= START_CUTOFF
  }

  def timeIsBeforeBedtime(start: Int): Boolean = {
    start <= BEDTIME
  }

  def timeIsAfterBedtime(time: Int): Boolean = {
    time >= BEDTIME
  }

  def timeIsBeforeMidnight(time: Int): Boolean = {
    time <= MIDNIGHT
  }

  def timeIsBetweenMidnightAndEndCutoff(time: Int): Boolean = {
    time == MIDNIGHT || timeIsBeforeEndCutoff(time)
  }

  def timeIsBeforeEndCutoff(time: Int): Boolean = {
    time <= END_CUTOFF
  }

  def timeIsAfterEndCutoff(time: Int): Boolean = {
    time >= END_CUTOFF
  }

  def payFromStartToBedtime(start: Int, end: Int): Int = {
    val startToBedtimePay: Int = 12
    if (timeIsAfterBedtime(start)) {
      0
    } else if (timeIsAfterStartCutoff(start) && timeIsAfterStartCutoff(end)) {
      val workedHours = start - end
      calculatePay(startToBedtimePay, workedHours)
    } else if (timeIsAfterStartCutoff(start) && timeIsAfterEndCutoff(end)) {
      val workedHours = start - (end + START_CUTOFF)
      calculatePay(startToBedtimePay, workedHours)
    } else if (timeIsAfterStartCutoff(start) && timeIsBeforeEndCutoff(end)) {
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
    } else if (timeIsBeforeBedtime(start) && (timeIsAfterBedtime(end) || timeIsBeforeEndCutoff(end))) {
      val hoursWorked = start - MIDNIGHT
      calculatePay(bedtimeToMidnightPay, hoursWorked)
    } else if (timeIsAfterBedtime(start) && timeIsBeforeMidnight(end)) {
      val hoursWorked = start - end
      calculatePay(bedtimeToMidnightPay, hoursWorked)
    } else if (timeIsBeforeMidnight(start) || (timeIsAfterMidnight(start) && timeIsBeforeEndCutoff(start))) {
      0
    } else {
      println(s"Pay from bedtime to midnight could not be calculated. Start: ${start}, End: ${end}")
      0
    }
  }

  def payFromMidnightToEnd(start: Int, end: Int): Int = {
    val midnightToEndPay: Int = 16
    if (timeIsAfterMidnight(start) && timeIsBeforeEndCutoff(start) && timeIsBeforeEndCutoff(end)) {
      val hoursWorked = hoursWorkedBetweenMidnightAndEnd(end)
      calculatePay(midnightToEndPay, hoursWorked)
    } else if (timeIsBetweenMidnightAndEndCutoff(start) && timeIsAfterEndCutoff(end)) {
      val hoursWorked = hoursWorkedBetweenMidnightAndEnd(end)
      calculatePay(midnightToEndPay, hoursWorked)
    } else if (timeIsAfterEndCutoff(start) && timeIsBeforeMidnight(start) && timeIsBeforeEndCutoff(end)) {
      calculatePay(midnightToEndPay, end)
    } else {
      println(s"Pay from midnight to end could not be calculated. Start: ${start}, End: ${end}")
      0
    }
  }
}

object BabysitterBoot extends BabysitterTools with App {

  println("What was your start time? Please enter UTC military time.")
  println("Ex: 5:00 p.m. '1700'")
  val inputStart: String = scala.io.StdIn.readLine().toString
  val start = setStartTime(inputStart.toInt)
  println("Start time entered: " + start)

  println("What was your end time? Please enter UTC military time.")
  println("Ex: 4:00 a.m. '400'")
  val inputEnd: String = scala.io.StdIn.readLine().toString
  val end: Int = setEndTime(inputEnd.toInt)
  println("End time entered: " + end)

  val payToBedtime: Int = payFromStartToBedtime(start, end)
  println("PayToBedtime: " + payToBedtime)

  val payToMidnight: Int = payFromBedtimeToMidnight(start, end)
  println("PayToMidnight: " + payToMidnight)

  val payToEnd: Int = payFromMidnightToEnd(start, end)
  println("PayToEnd: " + payToEnd)

  val totalPay: Int = payToBedtime + payToMidnight + payToEnd
  println("Your total pay is: " + totalPay)
}
