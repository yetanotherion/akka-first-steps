package com.spideo.hiring.ion.routes

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import com.spideo.hiring.ion.actors.AuctionHouseActor.{CreateAuction, UpdateAuction}
import com.spideo.hiring.ion.auction.AuctionTypes.{CreateAuctionAnswer, Item, Price, UpdateAuctionAnswer}

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

final case class AuctionRuleParams(startDate: String, endDate: String,
  item: Item, initialPrice: Price,
  increment: Int)

final case class AuctionRuleParamsUpdate(startDate: Option[String], endDate: Option[String],
  item: Option[Item], initialPrice: Option[Price],
  increment: Option[Int])

trait AuctionHouseRoutes extends JsonSupport {
  implicit def system: ActorSystem

  lazy val log = Logging(system, classOf[AuctionHouseRoutes])

  def auctionHouseActor: ActorRef

  implicit lazy val timeout = Timeout(5.seconds)

  lazy val auctionHouseRoutes: Route =
    pathPrefix("auctioneer" / IntNumber / "auction" / IntNumber) {
      (auctioneerId, auctionId) =>
        concat(
          pathEnd {
            concat(
              post {
                entity(as[AuctionRuleParams]) { auctionRuleParams =>
                  val (auctionCreated: Future[CreateAuctionAnswer]) =
                    (auctionHouseActor ? CreateAuction(auctioneerId, auctionId, auctionRuleParams)).mapTo[CreateAuctionAnswer]
                  onComplete(auctionCreated) {
                    case Success(answer) => complete((answer.status, answer.msg))
                    case Failure(ex) => complete((StatusCodes.InternalServerError, s"Got exception ${ex.getMessage()}"))
                  }
                }
              },
              put {
                entity(as[AuctionRuleParamsUpdate]) { auctionRuleParamsUpdate =>
                    val (auctionUpdated: Future[UpdateAuctionAnswer]) =
                      (auctionHouseActor ? UpdateAuction(auctioneerId, auctionId, auctionRuleParamsUpdate)).mapTo[UpdateAuctionAnswer]
                    onComplete(auctionUpdated) {
                      case Success(answer) => answer.msg match {
                        case Left(r) => complete((answer.status, r))
                        case Right(r) => complete((answer.status, r))
                      }
                      case Failure(ex) => complete((StatusCodes.InternalServerError, s"Got exception ${ex.getMessage()}"))
                    }
                }
              }
            )
          },
        )
    }
}

