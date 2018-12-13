package com.spideo.hiring.ion.actor

import akka.actor.Props
import com.ion.trials.akka.actors.{AuctionActor, AuctionActorBase}
import com.ion.trials.akka.auction.AuctionTypes.{
  AuctionId,
  AuctionRule,
  AuctioneerId
}

object AuctionActorInTest {
  def props(time: TestingTime,
            auctioneerId: AuctioneerId,
            auctionId: AuctionId,
            rule: AuctionRule): Props =
    Props(new AuctionActorInTest(time, auctioneerId, auctionId, rule))
}

class AuctionActorInTest(time: TestingTime,
                         auctioneerId: AuctioneerId,
                         auctionId: AuctionId,
                         rule: AuctionRule)
    extends AuctionActorBase(time = time,
                             auctioneerId = auctioneerId,
                             auctionId = auctionId,
                             rule = rule)

class TestingTime(var currentTime: Long) extends AuctionActor.Time {

  def getCurrentTime() = currentTime

  def advanceCurrentTime(shift: Long): Unit = {
    currentTime += shift
  }

  def setCurrentTime(currTime: Long): Unit = {
    currentTime = currTime
  }
}
