package com.ion.trials.akka.it

import akka.actor.ActorRef
import akka.http.scaladsl.model.{ContentTypes, StatusCodes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.ion.trials.akka.actor.AuctionHouseActorInTest
import com.ion.trials.akka.auction.AuctionTypes
import com.ion.trials.akka.auction.AuctionTypes.{
  AuctionDate,
  AuctionInfo,
  AuctionKey
}
import com.ion.trials.akka.routes.{AuctionHouseRoutes, AuctionRuleParams}
import com.ion.trials.akka.util.{AuctionTestData, TestingTime}
import org.scalatest.{Matchers, WordSpec}

class ServerSpec
    extends WordSpec
    with Matchers
    with ScalatestRouteTest
    with AuctionHouseRoutes
    with AuctionTestData {

  "Auctioneer API" should {
    "create an auction on POST /auctioneer/id/auction" in {

      val rule = AuctionRuleParams(
        startDate = AuctionTypes.fromAuctionDate(AuctionDate(startTime)),
        endDate = AuctionTypes.fromAuctionDate(AuctionDate(endTime)),
        item = 1,
        initialPrice = 0,
        increment = 1
      )

      val request = Post("/auctioneer/1/auction/1", rule)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)

        contentType should ===(ContentTypes.`application/json`)
        entityAs[AuctionInfo] should ===(
          createExpectedAuctionInfo(
            AuctionKey(auctioneerId = 1, auctionId = 1)))
      }
    }
  }

  private val testingTime = new TestingTime(currentTime)

  val auctionHouseActor: ActorRef =
    system.actorOf(AuctionHouseActorInTest.props(testingTime),
                   "auctionHouseActor")

  val routes = auctionHouseRoutes

}
