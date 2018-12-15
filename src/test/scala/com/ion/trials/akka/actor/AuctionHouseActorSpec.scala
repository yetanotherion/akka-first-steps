package com.ion.trials.akka.actor

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.testkit.{ImplicitSender, TestKit}
import com.ion.trials.akka.actors.AuctionHouseActor
import com.ion.trials.akka.actors.AuctionHouseActor.GetAuctions
import com.ion.trials.akka.actors.GatherAuctionsActor.AuctionInfos
import com.ion.trials.akka.auction.AuctionTypes.Answer
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.duration._
import scala.language.postfixOps

class AuctionHouseActorSpec
    extends TestKit(ActorSystem("AkkaAuctionHouseTest"))
    with Matchers
    with WordSpecLike
    with ImplicitSender
    with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  override def withFixture(test: NoArgTest) = {
    try super.withFixture(test)
    finally {
      auctionHouse match {
        case None => ()
        case Some(a) => {
          system.stop(a)
        }
      }
    }
  }

  "An AuctionHouse state" should {
    "update its item" in {
      val auctionHouse = createAuctionHouse()
      auctionHouse ! GetAuctions
      expectMsg(expectedMsgTimeout,
                Answer(StatusCodes.OK, Left(AuctionInfos(List()))))
    }
  }

  private var auctionHouse: Option[ActorRef] = None
  private val expectedMsgTimeout = 500 millis

  private def createAuctionHouse(): ActorRef = {
    system.actorOf(AuctionHouseActor.props())
  }
}
