package com.ion.trials.akka.service

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Directives.{get, pathPrefix, pathEnd}
import akka.http.scaladsl.server.{Route, RouteConcatenation}
import com.ion.trials.akka.actors.AuctionHouseActor.{GetAuctioneersAuctions}
import com.ion.trials.akka.actors.GatherAuctionsActor.AuctionInfos
import com.ion.trials.akka.auction.AuctionTypes.Answer
import akka.pattern.ask
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.media.{Content, Schema}
import io.swagger.v3.oas.annotations.responses.ApiResponse
import javax.ws.rs.{GET, Path}

import scala.concurrent.{ExecutionContext, Future}

@Path("/auctioneer/<auctioneerId>")
class AuctioneerService(auctionHouseActor: ActorRef, system: ActorSystem)(
    implicit executionContext: ExecutionContext)
    extends ServiceHelper
    with RouteConcatenation {

  lazy val log = Logging(system, classOf[AuctioneersService])

  @GET
  @Operation(
    summary = "Return auctions created by auctioneerId",
    description = "Return auctions created by auctioneerId",
    responses = Array(
      new ApiResponse(
        responseCode = "200",
        description = "Auctions",
        content = Array(
          new Content(
            schema = new Schema(implementation = classOf[AuctionInfos])))),
      new ApiResponse(responseCode = "500",
                      description = "Internal server error")
    )
  )
  def getAuctioneer: Route =
    pathPrefix("auctioneer" / IntNumber) { auctioneerId =>
      pathEnd {
        get {
          val (auctions: Future[Answer[AuctionInfos]]) =
            (auctionHouseActor ? GetAuctioneersAuctions(auctioneerId))
              .mapTo[Answer[AuctionInfos]]
          completeAuctionInfosAnswer(auctions)
        }
      }
    }

  def routes = getAuctioneer
}
