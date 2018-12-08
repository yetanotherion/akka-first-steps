package com.spideo.hiring.ion.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.http.scaladsl.model.StatusCodes
import com.spideo.hiring.ion.actors.Auction.{GetMessage, OpennedMessage, PlannedMessage}
import com.spideo.hiring.ion.auction.AuctionTypes._
import com.spideo.hiring.ion.auction.{Auctioneer, Planned}
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

  final case object StartAuction

  final case class CreateAuction(auctioneerId: AuctioneerId, auctionId: AuctionId, auctionRule: AuctionRuleParams)

  final case class UpdateAuction(auctioneerId: AuctioneerId, auctionId: AuctionId, auctionRule: AuctionRuleParamsUpdate)

  final case class GetAuction(auctioneerId: AuctioneerId, auctionId: AuctionId)

  final case class AddBidder(auctioneerId: AuctioneerId, auctionId: AuctionId, bidder: Bidder)

  final case class AddBid(auctioneerId: AuctioneerId, auctionId: AuctionId, bid: Bid)

  //val errorInCreateAuction = CreateAuctionAnswer(None, msg="Got an error")
}

class AuctionHouseActor extends Actor with ActorLogging {

  import AuctionHouseActor._

  override def preStart(): Unit = log.info("AuctionHouse started")

  override def postStop(): Unit = log.info("AuctionHouse stopped")

  val auctioneers = scala.collection.mutable.HashMap.empty[AuctioneerId, Auctioneer]
  // cache to know which bidders may be associated to which actor
  val bidders = scala.collection.mutable.HashMap.empty[Bidder, scala.collection.mutable.Set[ActorRef]]

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
        bidders.getOrElseUpdate(bidder, scala.collection.mutable.Set[ActorRef]()).add(actor)
        OpennedMessage(NewBidder(bidder))
      }
      forwardToActor(auctioneerId, auctionId, onForward)
    case AddBid(auctioneerId, auctionId, bid) =>
      forwardToActor(auctioneerId, auctionId, _ => OpennedMessage(NewBid(bid)))
    case GetAuction(auctioneerId, auctionId) =>
      forwardToActor(auctioneerId, auctionId, _ => GetMessage)

  }

  private def forwardToActor(auctioneerId: AuctioneerId, auctionId: AuctionId, message: ActorRef => Auction.Message): Unit = {
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
        val auctionProps = Auction.props(auctioneerId=auctioneerId, auctionId=auctionId, rule=auctionRule)
        val res = context.actorOf(auctionProps)
        getAuctioneer(auctioneerId).add(auctionId, res)
        true
    }
  }
}
