package com.spideo.hiring.ion.routes

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import com.spideo.hiring.ion.actors.AuctionHouseActor.{AddBid, AddBidder, CreateAuction, GetAuction, UpdateAuction}
import com.spideo.hiring.ion.auction.AuctionTypes._

import scala.concurrent.duration._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
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

  def completeAnswer(answer: Future[Answer[BidsOfBidder]]) = {
    onComplete(answer) {
      case Success(answer) => answer.msg match {
        case Left(r) => complete((answer.status, r))
        case Right(r) => complete((answer.status, r))
      }
      case Failure(ex) => complete((StatusCodes.InternalServerError, s"Got exception ${ex.getMessage()}"))
    }
  }

  def completeAuctionAnswer(answer: Future[AuctionAnswer]) = {
    onComplete(answer) {
      case Success(answer) => answer.msg match {
        case Left(r) => complete((answer.status, r))
        case Right(r) => complete((answer.status, r))
      }
      case Failure(ex) => complete((StatusCodes.InternalServerError, s"Got exception ${ex.getMessage()}"))
    }
  }

  lazy val auctioneerRoutes: Route =
    pathPrefix("auctioneer" / IntNumber / "auction" / IntNumber) {
      (auctioneerId, auctionId) =>
        concat(
          pathEnd {
            concat(
              post {
                entity(as[AuctionRuleParams]) { auctionRuleParams =>
                  val (auctionCreated: Future[AuctionAnswer]) =
                    (auctionHouseActor ? CreateAuction(auctioneerId, auctionId, auctionRuleParams))
                      .mapTo[AuctionAnswer]
                  completeAuctionAnswer(auctionCreated)
                }
              },
              put {
                entity(as[AuctionRuleParamsUpdate]) { auctionRuleParamsUpdate =>
                  val (auctionUpdated: Future[AuctionAnswer]) =
                    (auctionHouseActor ? UpdateAuction(auctioneerId, auctionId, auctionRuleParamsUpdate))
                      .mapTo[AuctionAnswer]
                  completeAuctionAnswer(auctionUpdated)
                }
              },
              get {
                val (auctionUpdated: Future[AuctionAnswer]) =
                  (auctionHouseActor ? GetAuction(auctioneerId, auctionId)).mapTo[AuctionAnswer]
                completeAuctionAnswer(auctionUpdated)
              }
            )
          },
          pathPrefix("bidder" / IntNumber) { bidderId =>
            concat(
              pathEnd {
                concat(
                  put {
                    val (auctionUpdated: Future[AuctionAnswer]) =
                      (auctionHouseActor ? AddBidder(auctioneerId, auctionId, bidderId)).mapTo[AuctionAnswer]
                    completeAuctionAnswer(auctionUpdated)
                  },
                  post {
                    entity(as[BidParam]) { bidParam =>
                      val bid = Bid(bidder = bidderId, price = bidParam.bid)
                      val (auctionUpdated: Future[AuctionAnswer]) =
                        (auctionHouseActor ? AddBid(auctioneerId, auctionId, bid)).mapTo[AuctionAnswer]
                      completeAuctionAnswer(auctionUpdated)
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
          completeAnswer(bidderBids)
        }
      }

  lazy val auctionHouseRoutes = auctioneerRoutes ~ bidderRoutes
}

