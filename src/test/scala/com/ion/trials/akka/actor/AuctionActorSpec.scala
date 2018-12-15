package com.ion.trials.akka.actor

import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.ion.trials.akka.actors.AuctionActor
import com.ion.trials.akka.actors.AuctionActor.{
  GetMessage,
  OpennedMessage,
  PlannedMessage
}
import com.ion.trials.akka.auction.AuctionTypes
import com.ion.trials.akka.auction.AuctionTypes._
import com.ion.trials.akka.auction.Openned.{NewBid, NewBidder}
import com.ion.trials.akka.util.{AuctionTestData, TestingTime}

class AuctionActorSpec
    extends TestKit(ActorSystem("AkkaAuctionTest"))
    with Matchers
    with WordSpecLike
    with ImplicitSender
    with BeforeAndAfterAll
    with AuctionTestData {

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

  "An Auction in planned state" should {
    "update its item" in {
      val auction = createAuction()
      val expected = auctionInfo.copy(rule = auctionRule.copy(item = 2))
      auction ! PlannedMessage(emptyUpdate.copy(item = Some(2)))
      expectOkAuctionInfo(expected)
    }

    "update its startDate and become openned" in {
      val auction = createAuction()
      val newAuctionDate = AuctionDate(currentTime)
      val expected =
        auctionInfo.copy(rule = auctionRule.copy(startDate = newAuctionDate),
                         currentPrice = Some(0),
                         state = AuctionActor.openned)
      val startDate = AuctionTypes.fromAuctionDate(newAuctionDate)
      auction ! PlannedMessage(emptyUpdate.copy(startDate = Some(startDate)))
      expectOkAuctionInfo(expected)
    }

    "filter an invalid startDate" in {
      val auction = createAuction()
      val newAuctionDate = AuctionDate(endTime + 1)
      val startDate = AuctionTypes.fromAuctionDate(newAuctionDate)
      val endDate = AuctionTypes.fromAuctionDate(AuctionDate(endTime))
      auction ! PlannedMessage(emptyUpdate.copy(startDate = Some(startDate)))
      expectMsg(
        expectedMsgTimeout,
        Answer(
          StatusCodes.BadRequest,
          Right(Error(
            s"invalid request: newStartDate: $startDate > endDate: $endDate"))))
    }

    "not update its state with a single bad update parameter" in {
      val auction = createAuction()

      auction ! GetMessage
      expectOkAuctionInfo(auctionInfo)

      val newEndDate = "This cannot be parsed as a date"
      auction ! PlannedMessage(
        emptyUpdate.copy(startDate = Some(newEndDate), item = Some(42)))
      expectMsg(
        expectedMsgTimeout,
        Answer(
          StatusCodes.BadRequest,
          Right(Error(s"invalid request: '$newEndDate' is an invalid date"))))

      auction ! GetMessage
      expectOkAuctionInfo(auctionInfo)
    }

    "update to openned on the correct time" in {
      val milliSecAfterCurrTime = 2
      val newStartTime = currentTime + milliSecAfterCurrTime
      val newRule = auctionRule.copy(startDate = AuctionDate(newStartTime))
      val newInfo = auctionInfo.copy(rule = newRule)
      val auction = createAuction(newRule)
      auction ! GetMessage
      expectOkAuctionInfo(newInfo)

      auction.underlyingActor.time.advanceCurrentTime(milliSecAfterCurrTime)
      auction ! GetMessage
      expectOkAuctionInfo(
        newInfo.copy(state = AuctionActor.openned, currentPrice = Some(0)))
    }
  }

  "An Auction in openned state" should {

    "update to closed on the correct time" in {
      val milliSecAfterCurrTime = 2
      val newStartTime = currentTime
      val newEndTime = newStartTime + milliSecAfterCurrTime
      val newRule = auctionRule.copy(startDate = AuctionDate(newStartTime),
                                     endDate = AuctionDate(newEndTime))
      val newInfo = auctionInfo.copy(rule = newRule,
                                     state = AuctionActor.openned,
                                     currentPrice = Some(0))
      val auction = createAuction(newRule)
      auction ! GetMessage
      expectOkAuctionInfo(newInfo)

      auction.underlyingActor.time.advanceCurrentTime(milliSecAfterCurrTime)

      auction ! GetMessage
      expectOkAuctionInfo(
        newInfo.copy(state = AuctionActor.closed, currentPrice = None))
    }

    "accept a bidder" in {
      val (auction, expectedAuctionInfo) = createOpennedAuction()
      auction ! OpennedMessage(NewBidder(1))
      expectOkAuctionInfo(expectedAuctionInfo.copy(bidders = List(1)))
    }

    "accept a bid from a valid bidder" in {
      val (auction, expectedAuctionInfo) = createOpennedAuction()
      auction ! OpennedMessage(NewBidder(1))
      val newExpectedAuction = expectedAuctionInfo.copy(bidders = List(1))
      expectOkAuctionInfo(newExpectedAuction)
      val bid = Bid(bidder = 1, price = 3)
      auction ! OpennedMessage(NewBid(bid))
      expectOkAuctionInfo(
        newExpectedAuction.copy(bids = List(bid), currentPrice = Some(3)))
    }

    "refuse a bid from an valid bidder" in {
      val (auction, expectedAuctionInfo) = createOpennedAuction()
      auction ! OpennedMessage(NewBidder(1))
      val newExpectedAuction = expectedAuctionInfo.copy(bidders = List(1))
      expectOkAuctionInfo(newExpectedAuction)
      val bid = Bid(bidder = 1, price = 0)
      auction ! OpennedMessage(NewBid(bid))
      expectMsg(
        expectedMsgTimeout,
        Answer(StatusCodes.BadRequest,
               Right(Error("bidder: 1 does not respect increment rules"))))
    }

    "refuse a bid from a bidder that did not join the auction" in {
      val (auction, expectedAuctionInfo) = createOpennedAuction()
      val bid = Bid(bidder = 1, price = 0)
      auction ! OpennedMessage(NewBid(bid))
      expectMsg(expectedMsgTimeout,
                Answer(StatusCodes.BadRequest,
                       Right(Error("1 did not join the auction yet"))))
    }

    "accept a bid from two bidders and close with the correct winner" in {
      val (auction, expectedAuctionInfo) = createOpennedAuction()
      auction ! OpennedMessage(NewBidder(1))
      var newExpectedAuction = expectedAuctionInfo.copy(bidders = List(1))
      expectOkAuctionInfo(newExpectedAuction)
      auction ! OpennedMessage(NewBidder(2))
      newExpectedAuction = expectedAuctionInfo.copy(bidders = List(1, 2))
      expectOkAuctionInfo(newExpectedAuction)

      val bid = Bid(bidder = 1, price = 2)
      auction ! OpennedMessage(NewBid(bid))
      newExpectedAuction =
        newExpectedAuction.copy(bids = List(bid), currentPrice = Some(2))
      expectOkAuctionInfo(newExpectedAuction)

      val sndBid = Bid(bidder = 2, price = 4)
      auction ! OpennedMessage(NewBid(sndBid))
      newExpectedAuction = newExpectedAuction.copy(bids = List(sndBid, bid),
                                                   currentPrice = Some(4))
      expectOkAuctionInfo(newExpectedAuction)

      auction.underlyingActor.time.advanceCurrentTime(endTime - currentTime)
      auction ! GetMessage
      newExpectedAuction = newExpectedAuction.copy(state = AuctionActor.closed,
                                                   winner = Some(sndBid),
                                                   currentPrice = None)
      expectOkAuctionInfo(newExpectedAuction)
    }
  }

  "An Auction in closed state" should {
    "not accept any bid" in {
      val (auction, expectedAuctionInfo) = createOpennedAuction()
      auction ! OpennedMessage(NewBidder(1))
      expectOkAuctionInfo(expectedAuctionInfo.copy(bidders = List(1)))
      auction.underlyingActor.time.advanceCurrentTime(endTime - currentTime)
      auction ! OpennedMessage(NewBid(Bid(bidder = 1, price = 2)))
      expectMsg(
        expectedMsgTimeout,
        Answer(
          StatusCodes.BadRequest,
          Right(Error(
            s"Message not supported in current state '${AuctionActor.closed}'"))))
    }
  }

  private var auction: Option[ActorRef] = None

  private def createOpennedAuction()
    : Tuple2[TestActorRef[AuctionActorInTest], AuctionInfo] = {
    val auction = createAuction()
    val newAuctionDate = AuctionDate(currentTime)
    val expectedAuctionInfo =
      auctionInfo.copy(rule = auctionRule.copy(startDate = newAuctionDate),
                       currentPrice = Some(0),
                       state = AuctionActor.openned)
    val startDate = AuctionTypes.fromAuctionDate(newAuctionDate)
    auction ! PlannedMessage(emptyUpdate.copy(startDate = Some(startDate)))
    expectOkAuctionInfo(expectedAuctionInfo)
    (auction, expectedAuctionInfo) // XXX assumes messages are not lost
  }

  private def expectOkAuctionInfo(expectedAuctionInfo: AuctionInfo) {
    expectMsg(expectedMsgTimeout,
              Answer(StatusCodes.OK, Left(expectedAuctionInfo)))
  }

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
