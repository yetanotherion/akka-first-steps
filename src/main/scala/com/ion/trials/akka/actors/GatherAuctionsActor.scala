package com.ion.trials.akka.actors

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef, Props, ReceiveTimeout}
import akka.http.scaladsl.model.StatusCodes
import com.ion.trials.akka.actors.Auction.{GetAuctionInfo, GetAuctionInfoAnswer}
import com.ion.trials.akka.auction.AuctionTypes._

import scala.concurrent.duration.FiniteDuration

object GatherAuctionsActor {
  def props(): Props = Props(new GatherAuctionsActor())
  final case class GatherAuctionsActorRequest(auctions: List[Tuple2[AuctionKey, ActorRef]], respondTo: ActorRef)

  val timeout = new FiniteDuration(length = 5, unit = TimeUnit.SECONDS)
  final case class AuctionInfos(auctions: List[AuctionInfo])
}


class GatherAuctionsActor extends Actor with ActorLogging {
  import GatherAuctionsActor._

  override def preStart(): Unit = log.info("GatherAuctions started")

  override def postStop(): Unit = log.info("GatherAuctions stopped")

  override def receive = {
    case GatherAuctionsActorRequest(auctions, respondTo) => {
      auctions
        .map(_._2)
        .foreach {_ ! GetAuctionInfo }
      context.setReceiveTimeout(timeout = timeout)
      context.become(waitingForGetAuctionInfo(respondTo, auctions.map(_._1).toSet))
    }
  }

  private def waitingForGetAuctionInfo(
    respondTo: ActorRef, expectedAnswers: Set[AuctionKey],
    res: List[AuctionInfo] = List.empty): Receive =
  {
    case GetAuctionInfoAnswer(auctionInfo) => {
      val auctionKey = AuctionKey(auctionId = auctionInfo.auctionId, auctioneerId = auctionInfo.auctioneerId)
      handleGetInfoAnswer(respondTo, expectedAnswers - auctionKey, auctionInfo :: res)
    }
    case ReceiveTimeout =>
      respondTo ! Answer(StatusCodes.RequestTimeout, Right("All actors did not answer in time, please retry"))
      context stop self
  }

  private def handleGetInfoAnswer(
    respondTo: ActorRef, expectedAnswers: Set[AuctionKey], res: List[AuctionInfo]) =
  {
    expectedAnswers.isEmpty match {
      case true => {
        respondTo ! Answer(status = StatusCodes.OK, msg = Left(AuctionInfos(auctions = res)))
        context stop self
      }
      case false => {
        context.become(waitingForGetAuctionInfo(respondTo, expectedAnswers, res))
      }
    }
  }
}


