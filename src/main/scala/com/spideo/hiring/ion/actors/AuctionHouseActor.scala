package com.spideo.hiring.ion.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Timers}
import akka.http.scaladsl.model.StatusCodes
import com.spideo.hiring.ion.actors.Auction.{GetMessage, OpennedMessage, PlannedMessage}
import com.spideo.hiring.ion.actors.BidsOfBidderActor.{BidsOfBidderRequest, DeleteFromCache}
import com.spideo.hiring.ion.auction.AuctionTypes._
import com.spideo.hiring.ion.auction.{Auctioneer, BiddersToAuctions, Planned}
import com.spideo.hiring.ion.auction.Openned.{NewBid, NewBidder}
import com.spideo.hiring.ion.routes.{AuctionRuleParams, AuctionRuleParamsUpdate}

import scala.util.{Failure, Success, Try}

object AuctionHouseActor {
  def props(): Props = Props(new AuctionHouseActor)

  private def toAuctionRule(params: AuctionRuleParams): Try[AuctionRule] = {
    try {
      Success(AuctionRule(startDate = toAuctionDate(params.startDate), endDate = toAuctionDate(params.endDate),
        item = params.item, initialPrice = params.initialPrice, increment = toIncrement(params.increment)))
    } catch {
      case e: IllegalArgumentException => Failure(e)
    }
  }

  final case class CreateAuction(auctioneerId: AuctioneerId, auctionId: AuctionId, auctionRule: AuctionRuleParams)

  final case class UpdateAuction(auctioneerId: AuctioneerId, auctionId: AuctionId, auctionRule: AuctionRuleParamsUpdate)

  final case class GetAuction(auctioneerId: AuctioneerId, auctionId: AuctionId)

  final case class AddBidder(auctioneerId: AuctioneerId, auctionId: AuctionId, bidder: Bidder)

  final case class AddBid(auctioneerId: AuctioneerId, auctionId: AuctionId, bid: Bid)

  final case class RetryGetAuctionsRelatedToBidder(bidder: Bidder)

}

class AuctionHouseActor extends Actor with ActorLogging with Timers {

  import AuctionHouseActor._

  override def preStart(): Unit = log.info("AuctionHouse started")

  override def postStop(): Unit = log.info("AuctionHouse stopped")

  val auctioneers = scala.collection.mutable.HashMap.empty[AuctioneerId, Auctioneer]

  val biddersToAuctionCache = new BiddersToAuctions()

  override def receive = {

    case CreateAuction(auctioneerId, auctionId, auctionRuleParams) => {
      toAuctionRule(auctionRuleParams) match {
        case Success(auctionRule) =>
          val ok = createAuction(auctioneerId, auctionId, auctionRule)
          if (ok) {
            sender() ! AuctionAnswer(StatusCodes.Created, Left(Planned.toPlannedInfo(auctionRule)))
          } else {
            sender() ! AuctionAnswer(StatusCodes.Conflict, Right(
              s"Auction $auctionId was already created by $auctioneerId"))
          }
        case Failure(e) =>
          sender() ! AuctionAnswer(StatusCodes.BadRequest, Right(e.getMessage))
      }
    }
    case UpdateAuction(auctioneerId, auctionId, auctionRuleParamsUpdate) =>
      forwardToActor(auctioneerId, auctionId, _ => PlannedMessage(auctionRuleParamsUpdate))

    case AddBidder(auctioneerId, auctionId, bidder) =>
      def onForward(actor: ActorRef) = {
        // Add the bidder to the cache, even if it may get refused
        // (for example if the actor is not in the OpennedState)
        biddersToAuctionCache.addAuction(bidder, auctioneerId, auctionId, actor)
        OpennedMessage(NewBidder(bidder))
      }

      forwardToActor(auctioneerId, auctionId, onForward)

    case AddBid(auctioneerId, auctionId, bid) =>
      forwardToActor(auctioneerId, auctionId, _ => OpennedMessage(NewBid(bid)))

    case GetAuction(auctioneerId, auctionId) =>
      forwardToActor(auctioneerId, auctionId, _ => GetMessage)

    /* 1 -> N  */
    case GetBidsOfBidderRequest(bidder) => {
      val actors = biddersToAuctionCache.getActors(bidder)
      actors.isEmpty match {
        case true => sender() != Answer(StatusCodes.OK, Left(BidsOfBidder(bids = List())))
        case false => {
          /* this actor will stop by itself when done or upon timeout */
          val newActor = context.actorOf(BidsOfBidderActor.props(bidder), "BidsOfBidder")
          newActor ! BidsOfBidderRequest(bidder = bidder, auctions = actors, respondTo = sender)
        }
      }
    }

    case DeleteFromCache(notFound) => {
      biddersToAuctionCache.deleteAuctionFromBidder(bidder = notFound.bidder,
        auctionId = notFound.auctionId, auctioneerId = notFound.auctioneerId)
    }
  }

  private def forwardToActor(auctioneerId: AuctioneerId, auctionId: AuctionId, message: ActorRef => Auction.Message)
  : Unit =
  {
    val auctioneer = getAuctioneer(auctioneerId)
    auctioneer.get(auctionId) match {
      case Some(actor) => {
        actor forward message(actor)
      }
      case None => {
        sender() ! AuctionAnswer(StatusCodes.NotFound,
          Right(s"Auction $auctionId was not created by $auctioneerId"))
      }
    }
  }

  def getAuctioneer(auctioneerId: AuctioneerId): Auctioneer = {
    auctioneers.getOrElseUpdate(auctioneerId, new Auctioneer())
  }

  def createAuction(auctioneerId: AuctioneerId, auctionId: AuctionId, auctionRule: AuctionRule): Boolean = {
    val auctioneer = getAuctioneer(auctioneerId)
    auctioneer.get(auctionId) match {
      case Some(_) => false
      case None =>
        val auctionProps = Auction.props(auctioneerId = auctioneerId, auctionId = auctionId, rule = auctionRule)
        val res = context.actorOf(auctionProps)
        getAuctioneer(auctioneerId).add(auctionId, res)
        true
    }
  }
}
