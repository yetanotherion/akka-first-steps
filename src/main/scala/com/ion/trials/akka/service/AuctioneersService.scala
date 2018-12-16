package com.ion.trials.akka.service

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import akka.http.scaladsl.server.Directives.{delete, get, path}
import akka.http.scaladsl.server.{Route, RouteConcatenation}
import com.ion.trials.akka.actors.AuctionHouseActor.{
  DeleteAuctions,
  GetAuctions
}
import com.ion.trials.akka.actors.GatherAuctionsActor.AuctionInfos
import com.ion.trials.akka.auction.AuctionTypes.Answer
import akka.pattern.ask
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.media.{Content, Schema}
import io.swagger.v3.oas.annotations.responses.ApiResponse
import javax.ws.rs.{DELETE, GET, Path}

import scala.concurrent.{ExecutionContext, Future}

@Path("/auctioneer")
class AuctioneersService(auctionHouseActor: ActorRef, system: ActorSystem)(
    implicit executionContext: ExecutionContext)
    extends ServiceHelper
    with RouteConcatenation {

  lazy val log = Logging(system, classOf[AuctioneersService])

  @GET
  @Operation(
    summary = "Return auctions created by all auctioneers",
    description = "Return auctions created by all auctioneers",
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
    path("auctioneer") {
      get {
        val (auctions: Future[Answer[AuctionInfos]]) =
          (auctionHouseActor ? GetAuctions)
            .mapTo[Answer[AuctionInfos]]
        completeAuctionInfosAnswer(auctions)
      }
    }

  @DELETE
  @Operation(
    summary = "Delete all auctions",
    description = "Delete all auctions",
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
  def deleteAuctioneer: Route =
    path("auctioneer") {
      delete {
        val (auctions: Future[Answer[AuctionInfos]]) =
          (auctionHouseActor ? DeleteAuctions)
            .mapTo[Answer[AuctionInfos]]
        completeAuctionInfosAnswer(auctions)
      }
    }

  def routes = getAuctioneer ~ deleteAuctioneer
}
