package com.ion.trials.akka.actor

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import com.ion.trials.akka.actors.AuctionActor
import com.ion.trials.akka.actors.AuctionHouseActor.{
  AddBid,
  AddBidder,
  CreateAuction,
  GetAuctions
}
import com.ion.trials.akka.actors.GatherAuctionsActor.AuctionInfos
import com.ion.trials.akka.actors.GatherBidsOfBidderActor.BidsOfBidder
import com.ion.trials.akka.auction.AuctionTypes
import com.ion.trials.akka.auction.AuctionTypes._
import com.ion.trials.akka.auction.Planned.plannedStr
import com.ion.trials.akka.routes.AuctionRuleParams
import com.ion.trials.akka.util.TestingTime
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

  "An AuctionHouse communicating with one auction" should {
    "not have any auction initially" in {
      val auctionHouse = createAuctionHouse()
      auctionHouse ! GetAuctions
      expectMsg(expectedMsgTimeout,
                Answer(StatusCodes.OK, Left(AuctionInfos(List()))))
    }

    "create an auction" in {
      createAuctionsAndExpectCorrectMessages(List(firstAuctionKey))
    }

    "refuse to recreate an auction" in {
      val auctionHouse =
        createAuctionsAndExpectCorrectMessages(List(firstAuctionKey))

      auctionHouse ! CreateAuction(auctioneerId = firstAuctionKey.auctioneerId,
                                   auctionId = firstAuctionKey.auctionId,
                                   auctionRule = auctionRuleParams)
      expectMsg(expectedMsgTimeout,
                Answer(StatusCodes.Conflict,
                       Right(Error("Auction 0 was already created by 1"))))

    }
  }

  "An AuctionHouse communicating with multiple auctions" should {
    "create two auctions and bid in one" in {
      createTwoAuctionsBidInOneAndExpectCorrectMessages()
    }

    "gather all auctions after two are created" in {
      val auctionHouse = createTwoAuctionsBidInOneAndExpectCorrectMessages()
      auctionHouse ! GetAuctions
      expectMsg(
        expectedMsgTimeout,
        Answer(
          StatusCodes.OK,
          Left(
            AuctionInfos(List(
              createExpectedAuctionInfo(firstAuctionKey).copy(
                state = AuctionActor.openned,
                bids = List(Bid(bidder = 1, price = 1)),
                bidders = List(1),
                currentPrice = Some(1)
              ),
              createExpectedAuctionInfo(sndAuctionKey).copy(
                state = AuctionActor.openned,
                currentPrice = Some(0)
              )
            )))
        )
      )
    }

    "gather all auctions of a bidder after two are created" in {
      val auctionHouse = createTwoAuctionsBidInOneAndExpectCorrectMessages()
      auctionHouse ! GetBidsOfBidderRequest(1)
      expectMsg(
        expectedMsgTimeout,
        Answer(
          StatusCodes.OK,
          Left(
            BidsOfBidder(
              List(BidsOfBidderInOneAuction(
                bidder = 1,
                bidders = List(1),
                state = AuctionActor.openned,
                auctionId = firstAuctionKey.auctionId,
                auctioneerId = firstAuctionKey.auctioneerId,
                bestBid = Some(Bid(bidder = 1, price = 1)),
                executedBids = List(Bid(bidder = 1, price = 1))
              )))
          )
        )
      )
    }

  }

  private var auctionHouse: Option[ActorRef] = None
  private val expectedMsgTimeout = 500 millis
  private val currentTime = 10
  private val startTime = currentTime + 2
  private val endTime = startTime + 1

  private def createTwoAuctionsBidInOneAndExpectCorrectMessages()
    : TestActorRef[AuctionHouseActorInTest] = {
    val auctionHouse = createAuctionsAndExpectCorrectMessages(
      List(firstAuctionKey, sndAuctionKey))
    auctionHouse.underlyingActor.time
      .advanceCurrentTime(startTime - currentTime)

    auctionHouse ! AddBidder(firstAuctionKey.auctioneerId,
                             firstAuctionKey.auctionId,
                             1)

    val newInfo = createExpectedAuctionInfo(firstAuctionKey).copy(
      state = AuctionActor.openned,
      bidders = List(1),
      currentPrice = Some(0)
    )
    expectMsg(
      expectedMsgTimeout,
      Answer(StatusCodes.OK, Left(newInfo))
    )

    auctionHouse ! AddBid(firstAuctionKey.auctioneerId,
                          firstAuctionKey.auctionId,
                          Bid(bidder = 1, price = 1))
    expectMsg(expectedMsgTimeout,
              Answer(StatusCodes.OK,
                     Left(
                       newInfo.copy(bids = List(Bid(bidder = 1, price = 1)),
                                    currentPrice = Some(1)))))
    auctionHouse
  }

  private def createAuctionsAndExpectCorrectMessages(
      keys: List[AuctionKey]): TestActorRef[AuctionHouseActorInTest] = {
    val auctionHouse = createAuctionHouse()
    keys.foreach { key =>
      auctionHouse ! CreateAuction(auctioneerId = key.auctioneerId,
                                   auctionId = key.auctionId,
                                   auctionRule = auctionRuleParams)
      expectMsg(
        expectedMsgTimeout,
        Answer(StatusCodes.Created, Left(createExpectedAuctionInfo(key))))
    }
    auctionHouse
  }

  private def createExpectedAuctionInfo(key: AuctionKey): AuctionInfo = {
    AuctionInfo(rule = auctionRule,
                state = plannedStr,
                bidders = List(),
                bids = List(),
                winner = None,
                currentPrice = None,
                auctionId = key.auctionId,
                auctioneerId = key.auctioneerId)
  }

  private val firstAuctionKey = AuctionKey(auctioneerId = 1, auctionId = 0)
  private val sndAuctionKey = AuctionKey(auctioneerId = 2, auctionId = 0)

  private val auctionRuleParams = AuctionRuleParams(
    startDate = AuctionTypes.fromAuctionDate(AuctionDate(startTime)),
    endDate = AuctionTypes.fromAuctionDate(AuctionDate(endTime)),
    item = 1,
    initialPrice = 0,
    increment = 1
  )

  private val auctionRule = AuctionRule(
    startDate = AuctionDate(startTime),
    endDate = AuctionDate(endTime),
    item = 1,
    initialPrice = 0,
    increment = Increment(1)
  )

  private def createAuctionHouse(): TestActorRef[AuctionHouseActorInTest] = {
    val time = new TestingTime(currentTime)
    val res = TestActorRef[AuctionHouseActorInTest](
      AuctionHouseActorInTest
        .props(time))
    auctionHouse = Some(res)
    res
  }
}
