package com.ion.trials.akka.service

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import com.ion.trials.akka.auction.AuctionTypes._

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.MethodDirectives.post

import scala.concurrent.{ExecutionContext, Future}
import akka.pattern.ask
import com.ion.trials.akka.actors.AuctionHouseActor._

class AuctionHouseService(auctionHouseActor: ActorRef, system: ActorSystem)(
    implicit executionContext: ExecutionContext)
    extends ServiceHelper {

  lazy val log = Logging(system, classOf[AuctionHouseService])

  lazy val auctioneerRoutes: Route =
    path(
      "auctioneer" / IntNumber / "auction" / IntNumber / "bidder" / IntNumber) {
      (auctioneerId, auctionId, bidderId) =>
        concat(
          pathEnd {
            concat(
              put {
                val (auctionUpdated: Future[Answer[AuctionInfo]]) =
                  (auctionHouseActor ? AddBidder(auctioneerId,
                                                 auctionId,
                                                 bidderId))
                    .mapTo[Answer[AuctionInfo]]
                completeAuctionInfoAnswer(auctionUpdated)
              },
              post {
                entity(as[BidParam]) { bidParam =>
                  val bid =
                    Bid(bidder = bidderId, price = bidParam.bid)
                  val (auctionUpdated: Future[Answer[AuctionInfo]]) =
                    (auctionHouseActor ? AddBid(auctioneerId, auctionId, bid))
                      .mapTo[Answer[AuctionInfo]]
                  completeAuctionInfoAnswer(auctionUpdated)
                }
              },
            )
          }
        )
    }

  lazy val routes = auctioneerRoutes

}
