package com.spideo.hiring.ion.actor

import akka.actor.Props
import com.ion.trials.akka.actors.{AuctionActorBase}
import com.ion.trials.akka.auction.AuctionTypes.{
  AuctionId,
  AuctionRule,
  AuctioneerId
}

object AuctionActorInTest {
  def props(auctioneerId: AuctioneerId,
            auctionId: AuctionId,
            rule: AuctionRule): Props =
    Props(new AuctionActorInTest(auctioneerId, auctionId, rule))
}

class AuctionActorInTest(auctioneerId: AuctioneerId,
                         auctionId: AuctionId,
                         rule: AuctionRule)
    extends AuctionActorBase(auctioneerId = auctioneerId,
                             auctionId = auctionId,
                             rule = rule)
    with TestingTime

trait TestingTime {
  var currentTime = 0L

  def getCurrentTime() = currentTime

  def advanceCurrentTime(shift: Long): Unit = {
    currentTime += shift
  }

  def setCurrentTime(currTime: Long): Unit = {
    currentTime = currTime
  }
}
