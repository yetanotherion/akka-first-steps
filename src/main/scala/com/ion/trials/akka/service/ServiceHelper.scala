package com.ion.trials.akka.service

import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.http.scaladsl.server.Directives.onComplete
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.RouteDirectives.complete
import akka.util.Timeout
import com.ion.trials.akka.actors.GatherAuctionsActor.AuctionInfos
import com.ion.trials.akka.auction.AuctionTypes.{Answer, AuctionInfo}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.duration._

trait ServiceHelper extends JsonSupport {
  implicit lazy val timeout = Timeout(5.seconds)

  def completeAuctionInfosAnswer(answer: Future[Answer[AuctionInfos]]) =
    completeAnswer[AuctionInfos]((s, b) => complete(s, b), answer)

  def completeAuctionInfoAnswer(answer: Future[Answer[AuctionInfo]]) =
    completeAnswer[AuctionInfo]((s, b) => complete(s, b), answer)

  def completeAnswer[T](onLeft: (StatusCode, T) => Route,
                        answer: Future[Answer[T]]) = {
    onComplete(answer) {
      case Success(answer) =>
        answer.msg match {
          case Left(r)  => onLeft(answer.status, r)
          case Right(r) => complete((answer.status, r))
        }
      case Failure(ex) =>
        complete(
          (StatusCodes.InternalServerError,
           s"Got exception ${ex.getMessage()}"))
    }
  }

}
