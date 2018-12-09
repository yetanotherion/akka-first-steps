package com.spideo.hiring.ion.actors

import akka.actor.{Actor, ActorLogging, Props}
import akka.http.scaladsl.model.StatusCodes
import com.spideo.hiring.ion.auction.AuctionTypes._
import com.spideo.hiring.ion.auction.{AuctionTypes, Closed, Openned, Planned}

sealed trait State

final case class PlannedState(notStarted: Planned) extends State

final case class OpennedState(onGoingAuction: Openned) extends State

final case class ClosedState(endedAuction: Closed) extends State

object Auction {
  def props(auctioneerId: AuctioneerId, auctionId: AuctionId, rule: AuctionRule): Props = Props(
    new Auction(auctioneerId, auctionId, rule))

  sealed abstract class Message

  final case class PlannedMessage(plannedMessage: Planned.PlannedMessage) extends Message

  final case class OpennedMessage(opennedMessage: Openned.Message) extends Message

  final object GetMessage extends Message

  final object GetAuctionInfo extends Message

  final case class GetAuctionInfoAnswer(answer: AuctionInfo)

  def getCurrentTime(): Long = {
    System.currentTimeMillis / 1000
  }

}

class Auction(auctioneerId: AuctioneerId, auctionId: AuctionId, rule: AuctionRule) extends Actor with ActorLogging {

  import Auction._

  private var state: State = PlannedState(new Planned(rule=rule, auctioneerId=auctioneerId, auctionId=auctionId))

  override def preStart(): Unit = log.info("Auction started")

  override def postStop(): Unit = log.info("Auction stopped")

  override def receive: Receive = partialUpdateState.andThen(receiveMsg)

  /* This enables us not to create a timer to update the state.
   * Assuming all information from the auction comes through this actor,
   * all the answers require a message, and thus
   * the state will always be updated before answering something.
   */
  def partialUpdateState: PartialFunction[Any, Any] = {
    case msg => {
      updateState()
      msg
    }
  }

  def receiveMsg: Receive = {
    case PlannedMessage(plannedMessage) => {
      state match {
        case PlannedState(planned) =>
          sender() ! planned.receive(plannedMessage)
        case OpennedState(_) | ClosedState(_) => sender() ! messageNotSupportedAnswer
      }
    }

    case OpennedMessage(opennedMessage) => {
      state match {
        case OpennedState(openned) => {
          sender() ! openned.receive(opennedMessage)
        }
        case PlannedState(_) | ClosedState(_) => sender() ! messageNotSupportedAnswer
      }
    }

    case GetMessage => {
      state match {
        case PlannedState(planned) => {
          sender() ! Answer(StatusCodes.OK, Left(Planned.toPlannedInfo(planned)))
        }
        case OpennedState(openned) => {
          sender() ! Answer(StatusCodes.OK, Left(Openned.toOpennedInfo(openned)))
        }
        case ClosedState(closed) => {
          sender() ! Answer(StatusCodes.OK, Left(Closed.toClosedInfo(closed)))
        }
      }
    }

    case GetAuctionInfo => {
      val res = state match {
        case PlannedState(planned) => {
          Planned.toPlannedInfo(planned)
        }
        case OpennedState(openned) => {
          Openned.toOpennedInfo(openned)
        }
        case ClosedState(closed) => {
          Closed.toClosedInfo(closed)
        }
      }
      sender() ! GetAuctionInfoAnswer(res)
    }

    case GetBidsOfBidderRequest(bidder) => {
      state match {
        case PlannedState(planned) => {
          sender() ! None
        }
        case OpennedState(openned) => {
          sender() ! answerGetBidsOfBidder(bidder, openned.bidders.toSeq, openned.bids, state = "openned")
        }
        case ClosedState(closed) => {
          sender() ! answerGetBidsOfBidder(bidder, closed.bidders, closed.bids, state = "closed")
        }
      }
    }
  }

  def messageNotSupportedAnswer = Answer(StatusCodes.BadRequest,
    Right(s"Message not supported in current state $state"))

  private def updateState(): Unit = {
    val currentTime = getCurrentTime()
    state match {
      case PlannedState(notStarted) => {
        if (didEnd(currentTime, notStarted.rule)) {
          state = ClosedState(new Closed(new Openned(notStarted)))
        }
        else if (didStart(currentTime, notStarted.rule)) {
          state = OpennedState(new Openned(notStarted))
        }
      }
      case OpennedState(onGoing) => {
        if (didEnd(currentTime, onGoing.rule)) {
          state = ClosedState(new Closed(onGoing))
        }
      }
      case ClosedState(_) => ()
    }
  }

  private def answerGetBidsOfBidder(
    bidder: Bidder, bidders: Seq[AuctionTypes.Bidder], bids: List[Bid],
    state: String): GetBidsOfBidderRequestAnswer =
  {
    val res = bidders.contains(bidder) match {
      case false => Right(NotFound(bidder = bidder, auctionId = auctionId, auctioneerId = auctioneerId))
      case true => {
        Left(
          BidsOfBidderInOneAuction(
            bidder = bidder,
            state = state,
            bidders = bidders.toList,
            auctionId = auctionId,
            auctioneerId = auctioneerId,
            bestBid = bids.headOption,
            executedBids = bids)
        )
      }
    }
    GetBidsOfBidderRequestAnswer(res)
  }

  private def didEnd(currentTime: Long, rule: AuctionRule): Boolean = {
    currentTime >= rule.endDate.epochInSec
  }

  private def didStart(currentTime: Long, rule: AuctionRule): Boolean = {
    currentTime >= rule.startDate.epochInSec
  }

}



