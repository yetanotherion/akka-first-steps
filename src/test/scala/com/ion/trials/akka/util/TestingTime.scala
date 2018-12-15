package com.ion.trials.akka.util

import com.ion.trials.akka.auction.AuctionTime

class TestingTime(var currentTime: Long) extends AuctionTime.Time {

  def getCurrentTime() = currentTime

  def advanceCurrentTime(shift: Long): Unit = {
    currentTime += shift
  }

  def setCurrentTime(currTime: Long): Unit = {
    currentTime = currTime
  }
}
