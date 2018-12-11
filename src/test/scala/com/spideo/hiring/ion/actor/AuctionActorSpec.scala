package com.spideo.hiring.ion.actor

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.testkit.{ImplicitSender, TestKit}
import com.ion.trials.akka.actors.AuctionActor
import com.ion.trials.akka.actors.AuctionActor.{PlannedMessage}
import com.ion.trials.akka.auction.AuctionTypes
import com.ion.trials.akka.auction.AuctionTypes._
import com.ion.trials.akka.routes.AuctionRuleParamsUpdate

import scala.concurrent.duration._
import scala.language.postfixOps

class AuctionActorSpec
    extends TestKit(ActorSystem("AkkaAuctionTest"))
    with Matchers
    with WordSpecLike
    with ImplicitSender
    with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  private val auctioneerId = 1
  private val auctionId = 1
  private val currentTime = AuctionActor.getCurrentTime()

  private val auctionRule = AuctionRule(
    startDate = AuctionDate(currentTime + 10),
    endDate = AuctionDate(currentTime + 100),
    item = 1,
    initialPrice = 0,
    increment = AuctionTypes.toIncrement(1))

  private val auctionInfo = AuctionInfo(
    rule = auctionRule,
    state = "planned",
    bidders = List(),
    bids = List(),
    winner = None,
    currentPrice = None,
    auctionId = auctionId,
    auctioneerId = auctioneerId
  )

  private val emptyUpdate = AuctionRuleParamsUpdate(startDate = None,
                                                    endDate = None,
                                                    item = None,
                                                    initialPrice = None,
                                                    increment = None)

  private def createAuction(): ActorRef = {
    system
      .actorOf(
        AuctionActor.props(auctioneerId = auctioneerId,
                           auctionId = auctionId,
                           rule = auctionRule))
  }

  "An Auction" should {
    "update its item" in {
      val auction = createAuction()
      val expected = auctionInfo.copy(rule = auctionRule.copy(item = 2))
      auction ! PlannedMessage(emptyUpdate.copy(item = Some(2)))
      expectMsg(500 millis, Answer(StatusCodes.OK, Left(expected)))
    }

    "update its startDate and become openned" in {
      val auction = createAuction()
      val newAuctionDate = AuctionDate(currentTime - 1)
      val expected =
        auctionInfo.copy(rule = auctionRule.copy(startDate = newAuctionDate),
                         currentPrice = Some(0),
                         state = "openned")
      val startDate = AuctionTypes.fromAuctionDate(newAuctionDate)
      auction ! PlannedMessage(emptyUpdate.copy(startDate = Some(startDate)))
      expectMsg(500 millis, Answer(StatusCodes.OK, Left(expected)))
    }

  }

}
