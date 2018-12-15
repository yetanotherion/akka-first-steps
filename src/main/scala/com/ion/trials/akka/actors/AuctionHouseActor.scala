package com.ion.trials.akka.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Timers}
import akka.http.scaladsl.model.StatusCodes
import com.ion.trials.akka.actors.AuctionActor.{
  GetMessage,
  OpennedMessage,
  PlannedMessage
}
import com.ion.trials.akka.actors.BidsOfBidderActor.{
  BidsOfBidder,
  BidsOfBidderRequest,
  DeleteFromCache
}
import com.ion.trials.akka.actors.GatherAuctionsActor.{
  AuctionInfos,
  GatherAuctionsActorRequest
}
import com.ion.trials.akka.auction.AuctionTypes._
import com.ion.trials.akka.auction.Openned.{NewBid, NewBidder}
import com.ion.trials.akka.auction.{
  AuctionTime,
  Auctioneer,
  BiddersToAuctions,
  Planned
}
import com.ion.trials.akka.routes.{AuctionRuleParams, AuctionRuleParamsUpdate}

object AuctionHouseActor {
  def props(): Props = Props(new AuctionHouseActor)

  final case class CreateAuction(auctioneerId: AuctioneerId,
                                 auctionId: AuctionId,
                                 auctionRule: AuctionRuleParams)

  final case class UpdateAuction(auctioneerId: AuctioneerId,
                                 auctionId: AuctionId,
                                 auctionRule: AuctionRuleParamsUpdate)

  final case class GetAuction(auctioneerId: AuctioneerId, auctionId: AuctionId)

  final case class AddBidder(auctioneerId: AuctioneerId,
                             auctionId: AuctionId,
                             bidder: Bidder)

  final case class AddBid(auctioneerId: AuctioneerId,
                          auctionId: AuctionId,
                          bid: Bid)

  final case object GetAuctions

  final case class GetAuctioneersAuctions(auctioneerId: AuctioneerId)

}

class AuctionHouseActor
    extends AuctionHouseActorBase(time = AuctionTime.SystemTime)

class AuctionHouseActorBase(time: AuctionTime.Time)
    extends Actor
    with ActorLogging
    with Timers {

  import AuctionHouseActor._

  override def preStart(): Unit = log.info("AuctionHouse started")

  override def postStop(): Unit = log.info("AuctionHouse stopped")

  val auctioneers =
    scala.collection.mutable.HashMap.empty[AuctioneerId, Auctioneer]

  val biddersToAuctionCache = new BiddersToAuctions()

  override def receive = {

    case CreateAuction(auctioneerId, auctionId, auctionRuleParams) => {
      Planned.validateAuctionRuleParams(auctionRuleParams) match {
        case Left(auctionRule) =>
          val ok = createAuction(auctioneerId, auctionId, auctionRule)
          if (ok) {
            sender() ! Answer(StatusCodes.Created,
                              Left(
                                Planned.toPlannedInfo(
                                  new Planned(rule = auctionRule,
                                              auctionId = auctionId,
                                              auctioneerId = auctioneerId))))
          } else {
            sender() ! Answer(
              StatusCodes.Conflict,
              Right(
                Error(
                  s"Auction $auctionId was already created by $auctioneerId")))
          }
        case Right(e) =>
          sender() ! Answer(StatusCodes.BadRequest,
                            Right(Error(s"Got errors ${e.mkString("\n")}")))
      }
    }
    case UpdateAuction(auctioneerId, auctionId, auctionRuleParamsUpdate) =>
      forwardToActor(auctioneerId,
                     auctionId,
                     _ => PlannedMessage(auctionRuleParamsUpdate))

    case AddBidder(auctioneerId, auctionId, bidder) =>
      def onForward(actor: ActorRef) = {
        // Add the bidder to the cache, even if it may get refused
        // (for example if the actor is not in the OpennedState).
        // Will be deleted when getting the request from an actor
        // that knows the bidder does not belong there (see DeleteFromCache below)
        biddersToAuctionCache.addAuction(bidder, auctioneerId, auctionId, actor)
        OpennedMessage(NewBidder(bidder))
      }

      forwardToActor(auctioneerId, auctionId, onForward)

    case AddBid(auctioneerId, auctionId, bid) =>
      forwardToActor(auctioneerId, auctionId, _ => OpennedMessage(NewBid(bid)))

    case GetAuction(auctioneerId, auctionId) =>
      forwardToActor(auctioneerId, auctionId, _ => GetMessage)

    case DeleteFromCache(notFound) => {
      biddersToAuctionCache.deleteAuctionFromBidder(
        bidder = notFound.bidder,
        auctionId = notFound.auctionId,
        auctioneerId = notFound.auctioneerId)
    }

    /* 1 -> N  */
    case GetBidsOfBidderRequest(bidder) => {
      val actors = biddersToAuctionCache.getActors(bidder)
      actors.isEmpty match {
        case true =>
          sender() != Answer(StatusCodes.OK, Left(BidsOfBidder(bids = List())))
        case false => {
          /* this actor will stop by itself when done or upon timeout */
          val newActor =
            context.actorOf(BidsOfBidderActor.props(bidder), "BidsOfBidder")
          newActor ! BidsOfBidderRequest(bidder = bidder,
                                         auctions = actors,
                                         respondTo = sender)
        }
      }
    }

    case GetAuctions => {
      val auctions =
        auctioneers.values.foldLeft(List.empty[Tuple2[AuctionKey, ActorRef]]) {
          case (res, auctioneer) => res ::: auctioneer.getAllAuctions()
        }
      sendGetAuctionsQuery(auctions, sender)
    }

    case GetAuctioneersAuctions(auctioneerId) => {
      val auctions = auctioneers.get(auctioneerId) match {
        case None          => List()
        case Some(auction) => auction.getAllAuctions()
      }
      sendGetAuctionsQuery(auctions, sender)
    }

  }

  private def sendGetAuctionsQuery(auctions: List[Tuple2[AuctionKey, ActorRef]],
                                   sender: ActorRef): Unit = {
    /* this actor will stop by itself when done or upon timeout */
    auctions.isEmpty match {
      case true => sender ! Answer(StatusCodes.OK, Left(AuctionInfos(List())))
      case false => {
        val newActor =
          context.actorOf(GatherAuctionsActor.props(), "GatherAuctionsActor")
        newActor ! GatherAuctionsActorRequest(auctions = auctions,
                                              respondTo = sender)
      }
    }
  }

  private def forwardToActor(
      auctioneerId: AuctioneerId,
      auctionId: AuctionId,
      message: ActorRef => AuctionActor.Message): Unit = {
    val auctioneer = getAuctioneer(auctioneerId)
    auctioneer.get(auctionId) match {
      case Some(actor) => {
        actor forward message(actor)
      }
      case None => {
        sender() ! Answer(
          StatusCodes.NotFound,
          Right(Error(s"Auction $auctionId was not created by $auctioneerId")))
      }
    }
  }

  private def getAuctioneer(auctioneerId: AuctioneerId): Auctioneer = {
    auctioneers.getOrElseUpdate(auctioneerId, new Auctioneer(auctioneerId))
  }

  private def createAuction(auctioneerId: AuctioneerId,
                            auctionId: AuctionId,
                            auctionRule: AuctionRule): Boolean = {
    val auctioneer = getAuctioneer(auctioneerId)
    auctioneer.get(auctionId) match {
      case Some(_) => false
      case None =>
        val auctionProps = AuctionActor.props(time = time,
                                              auctioneerId = auctioneerId,
                                              auctionId = auctionId,
                                              rule = auctionRule)
        val res = context.actorOf(auctionProps)
        getAuctioneer(auctioneerId).add(auctionId, res)
        true
    }
  }

}
