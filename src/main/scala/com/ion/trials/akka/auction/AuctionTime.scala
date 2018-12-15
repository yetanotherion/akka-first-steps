package com.ion.trials.akka.auction

object AuctionTime {
  abstract class Time {
    def getCurrentTime(): Long
    def setCurrentTime(currTime: Long): Unit
    def advanceCurrentTime(shift: Long): Unit
  }

  def getCurrentTime() = System.currentTimeMillis()

  final object SystemTime extends Time {
    def getCurrentTime() = getCurrentTime()

    def setCurrentTime(currTime: Long) = {
      throw new RuntimeException("setCurrentTime not supported")
    }

    def advanceCurrentTime(shift: Long) = {
      throw new RuntimeException("advanceCurrentTime not supported")
    }
  }

}
