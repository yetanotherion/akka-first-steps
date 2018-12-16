package com.ion.trials.akka.service

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import akka.http.scaladsl.server.directives.RouteDirectives.complete
import com.ion.trials.akka.actors.GatherBidsOfBidderActor.BidsOfBidder
import com.ion.trials.akka.auction.AuctionTypes.{Answer, GetBidsOfBidderRequest}
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask

import scala.concurrent.{ExecutionContext, Future}
import javax.ws.rs.{GET, Path}
import io.swagger.v3.oas.annotations.media.{Content, Schema}
import io.swagger.v3.oas.annotations.responses.ApiResponse
import io.swagger.v3.oas.annotations.{Operation}

@Path("/bidder/<bidderId>")
class BidderService(auctionHouseActor: ActorRef, system: ActorSystem)(
    implicit executionContext: ExecutionContext)
    extends ServiceHelper {

  lazy val log = Logging(system, classOf[BidInAuctionService])

  def completeBidsOfBidderAnswer(answer: Future[Answer[BidsOfBidder]]) =
    completeAnswer[BidsOfBidder]((s, b) => complete(s, b), answer)

  @GET
  @Operation(
    summary = "Return bids done by bidderId",
    description = "Return all the bids where the given bidder participated",
    responses = Array(
      new ApiResponse(
        responseCode = "200",
        description = "Bids response",
        content = Array(
          new Content(
            schema = new Schema(implementation = classOf[BidsOfBidder])))),
      new ApiResponse(responseCode = "500",
                      description = "Internal server error")
    )
  )
  def bidder =
    path("bidder" / IntNumber) { bidderId =>
      get {
        val (bidderBids: Future[Answer[BidsOfBidder]]) =
          (auctionHouseActor ? GetBidsOfBidderRequest(bidderId))
            .mapTo[Answer[BidsOfBidder]]
        completeBidsOfBidderAnswer(bidderBids)
      }
    }

  val routes = bidder
}
