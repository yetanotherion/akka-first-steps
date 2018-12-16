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
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.media.{Content, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.responses.ApiResponse
import javax.ws.rs.{POST, PUT, Path}

@Path("/auctioneer/<auctioneerId>/auction/<auctionId>/bidder/<bidderId>")
class BidInAuctionService(auctionHouseActor: ActorRef, system: ActorSystem)(
    implicit executionContext: ExecutionContext)
    extends ServiceHelper {

  lazy val log = Logging(system, classOf[BidInAuctionService])

  @POST
  @Operation(
    summary =
      "Make bidderId create a bid, note that bidderId must join the auction before (see PUT)",
    description = "\"Return the updated auction",
    requestBody = new RequestBody(
      content = Array(
        new Content(schema = new Schema(implementation = classOf[BidParam])))),
    responses = Array(
      new ApiResponse(
        responseCode = "200",
        description = "Auction",
        content = Array(
          new Content(
            schema = new Schema(implementation = classOf[AuctionInfo])))),
      new ApiResponse(responseCode = "500",
                      description = "Internal server error"),
      new ApiResponse(responseCode = "404", description = "Not found"),
      new ApiResponse(responseCode = "400", description = "Bad request")
    )
  )
  def postBid: Route =
    path(
      "auctioneer" / IntNumber / "auction" / IntNumber / "bidder" / IntNumber) {
      (auctioneerId, auctionId, bidderId) =>
        post {
          entity(as[BidParam]) { bidParam =>
            val bid =
              Bid(bidder = bidderId, price = bidParam.bid)
            val (auctionUpdated: Future[Answer[AuctionInfo]]) =
              (auctionHouseActor ? AddBid(auctioneerId, auctionId, bid))
                .mapTo[Answer[AuctionInfo]]
            completeAuctionInfoAnswer(auctionUpdated)
          }
        }
    }

  @PUT
  @Operation(
    summary = "Make bidderId join the auction",
    description = "\"Return the updated auction",
    responses = Array(
      new ApiResponse(
        responseCode = "200",
        description = "Auction",
        content = Array(
          new Content(
            schema = new Schema(implementation = classOf[AuctionInfo])))),
      new ApiResponse(responseCode = "500",
                      description = "Internal server error"),
      new ApiResponse(responseCode = "404", description = "Not found"),
    )
  )
  def putBid: Route =
    path(
      "auctioneer" / IntNumber / "auction" / IntNumber / "bidder" / IntNumber) {
      (auctioneerId, auctionId, bidderId) =>
        put {
          val (auctionUpdated: Future[Answer[AuctionInfo]]) =
            (auctionHouseActor ? AddBidder(auctioneerId, auctionId, bidderId))
              .mapTo[Answer[AuctionInfo]]
          completeAuctionInfoAnswer(auctionUpdated)
        }
    }

  lazy val routes = postBid ~ putBid

}
