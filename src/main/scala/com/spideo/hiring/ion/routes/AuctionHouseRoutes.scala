package com.spideo.hiring.ion.routes

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import com.spideo.hiring.ion.actors.AuctionHouseActor.{CreateAuction, CreateAuctionAnswer}
import com.spideo.hiring.ion.auction.AuctionTypes.Item

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
  item: Item, initialPrice: Item,
  increment: String)

trait AuctionHouseRoutes extends JsonSupport {
  // we leave these abstract, since they will be provided by the App
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
            )
          },
        )
    }
}

