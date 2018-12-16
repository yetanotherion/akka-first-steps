package com.ion.trials.akka.it

import akka.http.scaladsl.model.{ContentTypes, StatusCodes}
import akka.http.scaladsl.server.RouteConcatenation
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.testkit.TestKit
import com.ion.trials.akka.actor.AuctionHouseActorInTest
import com.ion.trials.akka.actors.AuctionActor
import com.ion.trials.akka.actors.GatherAuctionsActor.AuctionInfos
import com.ion.trials.akka.actors.GatherBidsOfBidderActor.BidsOfBidder
import com.ion.trials.akka.auction.AuctionTypes
import com.ion.trials.akka.auction.AuctionTypes._
import com.ion.trials.akka.service.AuctionService.{
  AuctionRuleParams,
  AuctionRuleParamsUpdate
}
import com.ion.trials.akka.service._
import com.ion.trials.akka.util.{AuctionTestData, TestingTime}
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Matchers, WordSpec}

class AuctionHouseServiceSpec
    extends WordSpec
    with Matchers
    with ScalatestRouteTest
    with BeforeAndAfterEach
    with BeforeAndAfterAll
    with JsonSupport
    with RouteConcatenation
    with AuctionTestData {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  override def afterEach() = {
    testingTime.setCurrentTime(currentTime)
    val request = Delete("/auctioneer")
    request ~> routes ~> check {
      status should ===(StatusCodes.OK)
      contentType should ===(ContentTypes.`application/json`)
    }
  }

  "Auctioneer API" should {
    "create an auction on POST /auctioneer/id/auction" in {
      createAuctionAndCheck()
    }

    "update the parameter of an auction on PUT /auctioneer/id/auction/id" in {
      createAuctionAndCheck()
      val request = Put(
        s"/auctioneer/${firstAuctionKey.auctioneerId}/auction/${firstAuctionKey.auctionId}",
        AuctionRuleParamsUpdate(startDate = None,
                                endDate = None,
                                item = Some(2),
                                initialPrice = None,
                                increment = None)
      )
      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        val expected = createExpectedAuctionInfo(firstAuctionKey)
        val rule = expected.rule
        entityAs[AuctionInfo] should ===(
          expected.copy(rule = rule.copy(item = 2)))
      }
    }

    "make a bidder join an auction on PUT /auctioneer/id/auction/id/bidder/id" in {
      createAuctionBidderAndCheck()
    }

    "make a bid on POST /auctioneer/id/auction/id/bidder/id" in {
      createAuctionBidderBidAndCheck()
    }

    "return the auction on GET /auctioneer/id/auction/id" in {
      createAuctionBidderBidAndCheck()
      val request = Get(
        s"/auctioneer/${firstAuctionKey.auctioneerId}/auction/${firstAuctionKey.auctionId}")
      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        entityAs[AuctionInfo] should ===(
          createExpectedAuctionInfo(firstAuctionKey)
            .copy(state = AuctionActor.openned,
                  bidders = List(1),
                  bids = List(Bid(bidder = 1, price = 3)),
                  currentPrice = Some(3)))
      }
    }

    "return all the auctions on GET /auctioneer" in {
      createAuctionBidderBidAndCheck()
      val request = Get(s"/auctioneer/${firstAuctionKey.auctioneerId}")
      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        entityAs[AuctionInfos] should ===(
          AuctionInfos(
            List(
              createExpectedAuctionInfo(firstAuctionKey)
                .copy(state = AuctionActor.openned,
                      bidders = List(1),
                      bids = List(Bid(bidder = 1, price = 3)),
                      currentPrice = Some(3)))))
      }
    }

    "return all the auctions a bidder did bid on GET /bidder/id" in {
      createAuctionBidderBidAndCheck()
      val request = Get(s"/bidder/1")
      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        entityAs[BidsOfBidder] should ===(
          BidsOfBidder(List(BidsOfBidderInOneAuction(
            bidder = 1,
            state = AuctionActor.openned,
            bidders = List(1),
            auctionId = firstAuctionKey.auctionId,
            auctioneerId = firstAuctionKey.auctioneerId,
            bestBid = Some(Bid(bidder = 1, price = 3)),
            executedBids = List(Bid(bidder = 1, price = 3))
          ))))
      }
    }
  }

  private def createAuctionBidderBidAndCheck(): Unit = {
    createAuctionBidderAndCheck()
    val request = Post(
      s"/auctioneer/${firstAuctionKey.auctioneerId}/auction/${firstAuctionKey.auctionId}/bidder/1",
      BidParam(bid = 3))
    request ~> routes ~> check {
      status should ===(StatusCodes.OK)

      contentType should ===(ContentTypes.`application/json`)
      entityAs[AuctionInfo] should ===(
        createExpectedAuctionInfo(firstAuctionKey)
          .copy(state = AuctionActor.openned,
                bidders = List(1),
                bids = List(Bid(bidder = 1, price = 3)),
                currentPrice = Some(3)))
    }
  }

  private def createAuctionBidderAndCheck(): Unit = {
    createAuctionAndCheck()
    val request = Put(
      s"/auctioneer/${firstAuctionKey.auctioneerId}/auction/${firstAuctionKey.auctionId}/bidder/1")
    testingTime.advanceCurrentTime(startTime - currentTime)
    request ~> routes ~> check {
      status should ===(StatusCodes.OK)

      contentType should ===(ContentTypes.`application/json`)
      entityAs[AuctionInfo] should ===(
        createExpectedAuctionInfo(firstAuctionKey)
          .copy(state = AuctionActor.openned,
                bidders = List(1),
                currentPrice = Some(0)))
    }
  }

  private def createAuctionAndCheck(): Unit = {
    val rule = AuctionRuleParams(
      startDate = AuctionTypes.fromAuctionDate(AuctionDate(startTime)),
      endDate = AuctionTypes.fromAuctionDate(AuctionDate(endTime)),
      item = 1,
      initialPrice = 0,
      increment = 1
    )

    val request = Post(
      s"/auctioneer/${firstAuctionKey.auctioneerId}/auction/${firstAuctionKey.auctionId}",
      rule)

    request ~> routes ~> check {
      status should ===(StatusCodes.Created)

      contentType should ===(ContentTypes.`application/json`)
      entityAs[AuctionInfo] should ===(
        createExpectedAuctionInfo(
          AuctionKey(auctioneerId = firstAuctionKey.auctioneerId,
                     auctionId = firstAuctionKey.auctionId)))
    }
  }

  // XXX unit tests cannot be run in // due this singleton
  private val testingTime = new TestingTime(currentTime)

  val auctionHouseActor =
    system.actorOf(AuctionHouseActorInTest.props(testingTime))

  val routes = (new AuctionHouseService(auctionHouseActor, system).routes
    ~ new BidderService(auctionHouseActor, system).routes
    ~ new AuctioneersService(auctionHouseActor, system).routes
    ~ new AuctioneerService(auctionHouseActor, system).routes
    ~ new AuctionService(auctionHouseActor, system).routes)

}
