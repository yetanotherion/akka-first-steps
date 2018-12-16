package com.ion.trials.akka.auction

object AuctionTime {
  abstract class Time {
    def getCurrentTime(): Long
    def setCurrentTime(currTime: Long): Unit
    def advanceCurrentTime(shift: Long): Unit
  }

  final object SystemTime extends Time {
    def getCurrentTime() = System.currentTimeMillis()

    def setCurrentTime(currTime: Long) = {
      throw new RuntimeException("setCurrentTime not supported")
    }

    def advanceCurrentTime(shift: Long) = {
      throw new RuntimeException("advanceCurrentTime not supported")
    }
  }

}
