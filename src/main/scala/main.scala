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
