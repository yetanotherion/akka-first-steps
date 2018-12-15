package com.ion.trials.akka.actor

import akka.actor.Props
import com.ion.trials.akka.actors.{AuctionHouseActorBase}
import com.ion.trials.akka.util.TestingTime

object AuctionHouseActorInTest {
  def props(time: TestingTime): Props =
    Props(new AuctionHouseActorInTest(time))
}

class AuctionHouseActorInTest(time: TestingTime)
    extends AuctionHouseActorBase(time)
