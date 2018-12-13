package com.spideo.hiring.ion.actor

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.ion.trials.akka.actors.AuctionActor.{GetMessage, PlannedMessage}
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

  override def withFixture(test: NoArgTest) = {
    try super.withFixture(test)
    finally {
      auction match {
        case None => ()
        case Some(a) => {
          system.stop(a)
        }
      }
    }
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
      val newAuctionDate = AuctionDate(currentTime)
      val expected =
        auctionInfo.copy(rule = auctionRule.copy(startDate = newAuctionDate),
                         currentPrice = Some(0),
                         state = "openned")
      val startDate = AuctionTypes.fromAuctionDate(newAuctionDate)
      auction ! PlannedMessage(emptyUpdate.copy(startDate = Some(startDate)))
      expectMsg(500 millis, Answer(StatusCodes.OK, Left(expected)))
    }

    "filters an invalid startDate" in {
      val auction = createAuction()
      val newAuctionDate = AuctionDate(endTime + 1)
      val startDate = AuctionTypes.fromAuctionDate(newAuctionDate)
      val endDate = AuctionTypes.fromAuctionDate(AuctionDate(endTime))
      auction ! PlannedMessage(emptyUpdate.copy(startDate = Some(startDate)))
      expectMsg(
        500 millis,
        Answer(
          StatusCodes.BadRequest,
          Right(
            s"invalid request: newStartDate: $startDate > endDate: $endDate")))
    }

    "not update its state with a single bad update parameter" in {
      val auction = createAuction()
      val expectedAnswer = Answer(StatusCodes.OK, Left(auctionInfo))

      auction ! GetMessage
      expectMsg(500 millis, expectedAnswer)

      val newEndDate = "This cannot be parsed as a date"
      auction ! PlannedMessage(
        emptyUpdate.copy(startDate = Some(newEndDate), item = Some(42)))
      expectMsg(
        500 millis,
        Answer(StatusCodes.BadRequest,
               Right(s"invalid request: '$newEndDate' is an invalid date")))

      auction ! GetMessage
      expectMsg(500 millis, expectedAnswer)

    }

    "update to openned on the correct time" in {
      val milliSecAfterCurrTime = 2
      val newStartTime = currentTime + milliSecAfterCurrTime
      val newRule = auctionRule.copy(startDate = AuctionDate(newStartTime))
      val newInfo = auctionInfo.copy(rule = newRule)
      val auction = createAuction(newRule)
      val expectedAnswer = Answer(StatusCodes.OK, Left(newInfo))

      auction ! GetMessage
      expectMsg(500 millis, expectedAnswer)
      auction.underlyingActor.time.advanceCurrentTime(milliSecAfterCurrTime)
      auction ! GetMessage
      val newExpectedAnswer =
        Answer(StatusCodes.OK,
               Left(newInfo.copy(state = "openned", currentPrice = Some(0))))
      expectMsg(500 millis, newExpectedAnswer)

    }

    "update to closed on the correct time" in {
      val milliSecAfterCurrTime = 2
      val newStartTime = currentTime
      val newEndTime = newStartTime + milliSecAfterCurrTime
      val newRule = auctionRule.copy(startDate = AuctionDate(newStartTime),
                                     endDate = AuctionDate(newEndTime))
      val newInfo = auctionInfo.copy(rule = newRule,
                                     state = "openned",
                                     currentPrice = Some(0))
      val auction = createAuction(newRule)
      val expectedAnswer = Answer(StatusCodes.OK, Left(newInfo))
      auction ! GetMessage
      expectMsg(500 millis, expectedAnswer)

      auction.underlyingActor.time.advanceCurrentTime(milliSecAfterCurrTime)

      auction ! GetMessage
      val newExpectedAnswer =
        Answer(StatusCodes.OK,
               Left(newInfo.copy(state = "closed", currentPrice = None)))
      expectMsg(500 millis, newExpectedAnswer)

    }

  }

  private val auctioneerId = 1
  private val auctionId = 1
  private val currentTime = 10

  private val startTime = currentTime + 1
  private val endTime = startTime + 2

  private val auctionRule = AuctionRule(startDate = AuctionDate(startTime),
                                        endDate = AuctionDate(endTime),
                                        item = 1,
                                        initialPrice = 0,
                                        increment = AuctionTypes.toIncrement(1))

  private var auction: Option[ActorRef] = None

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

  private def createAuction(): TestActorRef[AuctionActorInTest] = {
    createAuction(rule = auctionRule)
  }

  private def createAuction(
      rule: AuctionRule): TestActorRef[AuctionActorInTest] = {
    val time = new TestingTime(currentTime)
    val res = TestActorRef[AuctionActorInTest](
      AuctionActorInTest
        .props(time,
               auctioneerId = auctioneerId,
               auctionId = auctionId,
               rule = rule))
    auction = Some(res)
    res
  }

}
