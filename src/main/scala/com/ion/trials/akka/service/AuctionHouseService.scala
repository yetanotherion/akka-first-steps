package com.ion.trials.akka.service

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import com.ion.trials.akka.auction.AuctionTypes._

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.MethodDirectives.post
import akka.http.scaladsl.server.directives.RouteDirectives.complete

import scala.concurrent.{ExecutionContext, Future}
import akka.pattern.ask
import com.ion.trials.akka.actors.AuctionHouseActor._
import com.ion.trials.akka.actors.GatherAuctionsActor.AuctionInfos

final case class AuctionRuleParams(startDate: String,
                                   endDate: String,
                                   item: Item,
                                   initialPrice: Price,
                                   increment: Int)

final case class AuctionRuleParamsUpdate(startDate: Option[String],
                                         endDate: Option[String],
                                         item: Option[Item],
                                         initialPrice: Option[Price],
                                         increment: Option[Int])

class AuctionHouseService(auctionHouseActor: ActorRef, system: ActorSystem)(
    implicit executionContext: ExecutionContext)
    extends ServiceHelper {

  lazy val log = Logging(system, classOf[AuctionHouseService])

  def completeAuctionInfoAnswer(answer: Future[Answer[AuctionInfo]]) =
    completeAnswer[AuctionInfo]((s, b) => complete(s, b), answer)

  lazy val auctioneerRoutes: Route =
    pathPrefix("auctioneer" / IntNumber) { auctioneerId =>
      concat(
        pathEnd {
          get {
            val (auctions: Future[Answer[AuctionInfos]]) =
              (auctionHouseActor ? GetAuctioneersAuctions(auctioneerId))
                .mapTo[Answer[AuctionInfos]]
            completeAuctionInfosAnswer(auctions)
          }
        },
        pathPrefix("auction" / IntNumber) { auctionId =>
          concat(
            pathEnd {
              concat(
                post {
                  entity(as[AuctionRuleParams]) { auctionRuleParams =>
                    val (auctionCreated: Future[Answer[AuctionInfo]]) =
                      (auctionHouseActor ? CreateAuction(auctioneerId,
                                                         auctionId,
                                                         auctionRuleParams))
                        .mapTo[Answer[AuctionInfo]]
                    completeAuctionInfoAnswer(auctionCreated)
                  }
                },
                put {
                  entity(as[AuctionRuleParamsUpdate]) {
                    auctionRuleParamsUpdate =>
                      val (auctionUpdated: Future[Answer[AuctionInfo]]) =
                        (auctionHouseActor ? UpdateAuction(
                          auctioneerId,
                          auctionId,
                          auctionRuleParamsUpdate))
                          .mapTo[Answer[AuctionInfo]]
                      completeAuctionInfoAnswer(auctionUpdated)
                  }
                },
                get {
                  val (auctionUpdated: Future[Answer[AuctionInfo]]) =
                    (auctionHouseActor ? GetAuction(auctioneerId, auctionId))
                      .mapTo[Answer[AuctionInfo]]
                  completeAuctionInfoAnswer(auctionUpdated)
                }
              )
            },
            pathPrefix("bidder" / IntNumber) {
              bidderId =>
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
                            (auctionHouseActor ? AddBid(auctioneerId,
                                                        auctionId,
                                                        bid))
                              .mapTo[Answer[AuctionInfo]]
                          completeAuctionInfoAnswer(auctionUpdated)
                        }
                      },
                    )
                  }
                )
            }
          )
        }
      )
    }

  lazy val routes = auctioneerRoutes

}
