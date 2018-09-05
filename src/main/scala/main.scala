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
}

object BabysitterBoot extends BabysitterTools with App {

  println("What was your start time? Please enter UTC military time.")
  println("Ex: 5:00 p.m. '1700'")
  val inputStart: String = scala.io.StdIn.readLine().toString
  val validStart = validateStartTime(inputStart.toInt)
  println("validStart: " + validStart)
}
