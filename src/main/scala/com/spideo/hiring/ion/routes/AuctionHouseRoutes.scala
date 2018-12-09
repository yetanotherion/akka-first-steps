package com.spideo.hiring.ion.routes

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.spideo.hiring.ion.actors.AuctionHouseActor.{AddBid, AddBidder, CreateAuction, GetAuction, UpdateAuction}
import com.spideo.hiring.ion.auction.AuctionTypes._

import scala.concurrent.duration._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.MethodDirectives.post
import akka.http.scaladsl.server.directives.RouteDirectives.complete

import scala.concurrent.Future
import akka.pattern.ask
import akka.util.Timeout

import scala.util.{Failure, Success}

final case class AuctionRuleParams(
  startDate: String, endDate: String,
  item: Item, initialPrice: Price,
  increment: Int)

final case class AuctionRuleParamsUpdate(
  startDate: Option[String], endDate: Option[String],
  item: Option[Item], initialPrice: Option[Price],
  increment: Option[Int])

trait AuctionHouseRoutes extends JsonSupport {
  implicit def system: ActorSystem

  lazy val log = Logging(system, classOf[AuctionHouseRoutes])

  def auctionHouseActor: ActorRef

  implicit lazy val timeout = Timeout(5.seconds)

  def completeAnswer[T](onLeft: (StatusCode, T) => Route, answer: Future[Answer[T]]) = {
    onComplete(answer) {
      case Success(answer) => answer.msg match {
        case Left(r) => onLeft(answer.status, r)
        case Right(r) => complete((answer.status, r))
      }
      case Failure(ex) => complete((StatusCodes.InternalServerError, s"Got exception ${ex.getMessage()}"))
    }
  }

  def completeBidsOfBidderAnswer(answer: Future[Answer[BidsOfBidder]]) = completeAnswer[BidsOfBidder]((s, b) => complete(s, b), answer)
  def completeAuctionInfoAnswer(answer: Future[Answer[AuctionInfo]]) = completeAnswer[AuctionInfo]((s, b) => complete(s, b), answer)

  lazy val auctioneerRoutes: Route =
    pathPrefix("auctioneer" / IntNumber / "auction" / IntNumber) {
      (auctioneerId, auctionId) =>
        concat(
          pathEnd {
            concat(
              post {
                entity(as[AuctionRuleParams]) { auctionRuleParams =>
                  val (auctionCreated: Future[Answer[AuctionInfo]]) =
                    (auctionHouseActor ? CreateAuction(auctioneerId, auctionId, auctionRuleParams))
                      .mapTo[Answer[AuctionInfo]]
                  completeAuctionInfoAnswer(auctionCreated)
                }
              },
              put {
                entity(as[AuctionRuleParamsUpdate]) { auctionRuleParamsUpdate =>
                  val (auctionUpdated: Future[Answer[AuctionInfo]]) =
                    (auctionHouseActor ? UpdateAuction(auctioneerId, auctionId, auctionRuleParamsUpdate))
                      .mapTo[Answer[AuctionInfo]]
                  completeAuctionInfoAnswer(auctionUpdated)
                }
              },
              get {
                val (auctionUpdated: Future[Answer[AuctionInfo]]) =
                  (auctionHouseActor ? GetAuction(auctioneerId, auctionId)).mapTo[Answer[AuctionInfo]]
                completeAuctionInfoAnswer(auctionUpdated)
              }
            )
          },
          pathPrefix("bidder" / IntNumber) { bidderId =>
            concat(
              pathEnd {
                concat(
                  put {
                    val (auctionUpdated: Future[Answer[AuctionInfo]]) =
                      (auctionHouseActor ? AddBidder(auctioneerId, auctionId, bidderId)).mapTo[Answer[AuctionInfo]]
                    completeAuctionInfoAnswer(auctionUpdated)
                  },
                  post {
                    entity(as[BidParam]) { bidParam =>
                      val bid = Bid(bidder = bidderId, price = bidParam.bid)
                      val (auctionUpdated: Future[Answer[AuctionInfo]]) =
                        (auctionHouseActor ? AddBid(auctioneerId, auctionId, bid)).mapTo[Answer[AuctionInfo]]
                      completeAuctionInfoAnswer(auctionUpdated)
                    }
                  },
                )
              }
            )
          }
        )
    }

  lazy val bidderRoutes =
    path("bidder" / IntNumber) { bidderId =>
        get {
          val (bidderBids: Future[Answer[BidsOfBidder]]) =
            (auctionHouseActor ? GetBidsOfBidderRequest(bidderId)).mapTo[Answer[BidsOfBidder]]
          completeBidsOfBidderAnswer(bidderBids)
        }
      }

  lazy val auctionHouseRoutes = auctioneerRoutes ~ bidderRoutes
}

