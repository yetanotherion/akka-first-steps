package com.ion.trials.akka.actors

import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorLogging, ActorRef, Props, ReceiveTimeout}
import akka.http.scaladsl.model.StatusCodes
import com.ion.trials.akka.auction.AuctionTypes._

import scala.concurrent.duration.FiniteDuration

object BidsOfBidderActor {
  def props(bidder: Bidder): Props = Props(new BidsOfBidderActor(bidder))
  final case class BidsOfBidderRequest(bidder: Bidder, auctions: List[Tuple2[AuctionKey, ActorRef]],
    respondTo: ActorRef)

  final case class DeleteFromCache(notFound: NotFound)
  val timeout = new FiniteDuration(length = 5, unit = TimeUnit.SECONDS)
  final case class BidsOfBidder(bids: List[BidsOfBidderInOneAuction])
}

class BidsOfBidderActor(val bidder: Bidder) extends Actor with ActorLogging {
  import BidsOfBidderActor._

  override def preStart(): Unit = log.info("BidsOfBidderActor started")

  override def postStop(): Unit = log.info("BidsOfBidderActor stopped")

  override def receive = {
    case BidsOfBidderRequest(bidder, auctions, respondTo) => {
      auctions
        .map(_._2)
        .foreach {_ ! GetBidsOfBidderRequest(bidder)}
      context.setReceiveTimeout(timeout = timeout)
      context.become(waitingForGetBidders(sender, respondTo, auctions.map(_._1).toSet))
    }
  }

  private def waitingForGetBidders(sender: ActorRef,
    respondTo: ActorRef, expectedAnswers: Set[AuctionKey],
    res: List[BidsOfBidderInOneAuction] = List.empty): Receive =
  {
    case GetBidsOfBidderRequestAnswer(answer) =>
      answer match {
        case Left(bidder) => {
          val auctionKey = AuctionKey(auctionId = bidder.auctionId, auctioneerId = bidder.auctioneerId)
          handleGetBiddersBidAnswer(respondTo, expectedAnswers - auctionKey, bidder :: res)
        }
        case Right(notFound) =>
          sender != DeleteFromCache(notFound)
          val auctionKey = AuctionKey(auctionId = notFound.auctionId, auctioneerId = notFound.auctioneerId)
          handleGetBiddersBidAnswer(respondTo, expectedAnswers - auctionKey, res)
      }
    case ReceiveTimeout =>
      respondTo ! Answer(StatusCodes.RequestTimeout, Right("All actors did not answer in time, please retry"))
      context stop self
  }

  private def handleGetBiddersBidAnswer(
    respondTo: ActorRef, expectedAnswers: Set[AuctionKey], res: List[BidsOfBidderInOneAuction]) =
  {
    expectedAnswers.isEmpty match {
      case true => {
        respondTo ! Answer(status = StatusCodes.OK, msg = Left(BidsOfBidder(bids = res)))
        context stop self
      }
      case false => {
        context.become(waitingForGetBidders(sender, respondTo, expectedAnswers, res))
      }
    }
  }
}
