package com.ion.trials.akka.service

import akka.actor.{ActorRef, ActorSystem}
import akka.event.Logging
import akka.http.scaladsl.server.{Route, RouteConcatenation}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.MethodDirectives.post
import com.ion.trials.akka.actors.AuctionHouseActor.{
  CreateAuction,
  GetAuction,
  UpdateAuction
}
import com.ion.trials.akka.auction.AuctionTypes.{
  Answer,
  AuctionInfo,
  Item,
  Price
}
import akka.pattern.ask
import com.ion.trials.akka.service.AuctionService.{
  AuctionRuleParams,
  AuctionRuleParamsUpdate
}
import io.swagger.v3.oas.annotations.Operation
import io.swagger.v3.oas.annotations.media.{Content, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.responses.ApiResponse
import javax.ws.rs.{GET, POST, PUT, Path}

import scala.concurrent.{ExecutionContext, Future}

object AuctionService {
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
}

@Path("/auctioneer/<auctioneerId>/auction/<auctionId>")
class AuctionService(auctionHouseActor: ActorRef, system: ActorSystem)(
    implicit executionContext: ExecutionContext)
    extends ServiceHelper
    with RouteConcatenation {

  lazy val log = Logging(system, classOf[AuctionService])

  @GET
  @Operation(
    summary = "Return the auction auctionId created by auctioneerId",
    description = "\"Return the auction auctionId created by auctioneerId\"",
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
  def getAuction: Route =
    path("auctioneer" / IntNumber / "auction" / IntNumber) {
      (auctioneerId, auctionId) =>
        get {
          val (auctionUpdated: Future[Answer[AuctionInfo]]) =
            (auctionHouseActor ? GetAuction(auctioneerId, auctionId))
              .mapTo[Answer[AuctionInfo]]
          completeAuctionInfoAnswer(auctionUpdated)
        }
    }

  @POST
  @Operation(
    summary = "Make auctioneerId create the auction auctionId",
    description = "\"Return the created auction",
    requestBody = new RequestBody(
      content = Array(new Content(
        schema = new Schema(implementation = classOf[AuctionRuleParams])))),
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
  def postAuction: Route =
    path("auctioneer" / IntNumber / "auction" / IntNumber) {
      (auctioneerId, auctionId) =>
        post {
          entity(as[AuctionRuleParams]) { auctionRuleParams =>
            val (auctionCreated: Future[Answer[AuctionInfo]]) =
              (auctionHouseActor ? CreateAuction(auctioneerId,
                                                 auctionId,
                                                 auctionRuleParams))
                .mapTo[Answer[AuctionInfo]]
            completeAuctionInfoAnswer(auctionCreated)
          }
        }
    }

  @PUT
  @Operation(
    summary = "Update parameters of auctionId created by auctioneerId",
    description = "\"Return the updated auction",
    requestBody = new RequestBody(content = Array(new Content(
      schema = new Schema(implementation = classOf[AuctionRuleParamsUpdate])))),
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
      new ApiResponse(responseCode = "400", description = "Bad request"),
    )
  )
  def putAuction: Route =
    path("auctioneer" / IntNumber / "auction" / IntNumber) {
      (auctioneerId, auctionId) =>
        put {
          entity(as[AuctionRuleParamsUpdate]) { auctionRuleParamsUpdate =>
            val (auctionUpdated: Future[Answer[AuctionInfo]]) =
              (auctionHouseActor ? UpdateAuction(auctioneerId,
                                                 auctionId,
                                                 auctionRuleParamsUpdate))
                .mapTo[Answer[AuctionInfo]]
            completeAuctionInfoAnswer(auctionUpdated)
          }
        }
    }

  def routes = getAuction ~ postAuction ~ putAuction
}
