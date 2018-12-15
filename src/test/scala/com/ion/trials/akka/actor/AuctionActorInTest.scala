package com.ion.trials.akka.actor

import akka.actor.Props
import com.ion.trials.akka.actors.AuctionActor
import com.ion.trials.akka.auction.AuctionTypes.{
  AuctionId,
  AuctionRule,
  AuctioneerId
}
import com.ion.trials.akka.util.TestingTime

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
    extends AuctionActor(time = time,
                         auctioneerId = auctioneerId,
                         auctionId = auctionId,
                         rule = rule)
