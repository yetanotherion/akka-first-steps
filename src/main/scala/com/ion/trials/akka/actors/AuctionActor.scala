package com.ion.trials.akka.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.http.scaladsl.model.{StatusCodes}
import com.ion.trials.akka.auction.{AuctionTypes, Closed, Openned, Planned}
import com.ion.trials.akka.auction.AuctionTypes._

sealed trait State

final case class PlannedState(notStarted: Planned) extends State

final case class OpennedState(onGoingAuction: Openned) extends State

final case class ClosedState(endedAuction: Closed) extends State

object AuctionActor {
  def props(auctioneerId: AuctioneerId,
            auctionId: AuctionId,
            rule: AuctionRule): Props =
    Props(new AuctionActor(auctioneerId, auctionId, rule))

  sealed abstract class Message

  final case class PlannedMessage(plannedMessage: Planned.PlannedMessage)
      extends Message

  final case class OpennedMessage(opennedMessage: Openned.Message)
      extends Message

  final object GetMessage extends Message
  final case class GetMessageFrom(actor: ActorRef) extends Message

  final object GetAuctionInfo extends Message

  final case class GetAuctionInfoAnswer(answer: AuctionInfo)

  def getCurrentTime() = System.currentTimeMillis()

  abstract class Time {
    def getCurrentTime(): Long
    def setCurrentTime(currTime: Long): Unit
    def advanceCurrentTime(shift: Long): Unit
  }

  final object SystemTime extends AuctionActor.Time {
    def getCurrentTime() = AuctionActor.getCurrentTime()
    def setCurrentTime(currTime: Long) = {
      throw new RuntimeException("setCurrentTime not supported")
    }
    def advanceCurrentTime(shift: Long) = {
      throw new RuntimeException("advanceCurrentTime not supported")
    }
  }

}

class AuctionActor(auctioneerId: AuctioneerId,
                   auctionId: AuctionId,
                   rule: AuctionRule)
    extends AuctionActorBase(time = AuctionActor.SystemTime,
                             auctioneerId = auctioneerId,
                             auctionId = auctionId,
                             rule = rule)

class AuctionActorBase(val time: AuctionActor.Time,
                       auctioneerId: AuctioneerId,
                       auctionId: AuctionId,
                       rule: AuctionRule)
    extends Actor
    with ActorLogging {

  import AuctionActor._

  def getCurrentTime() = time.getCurrentTime()

  private var state: State = PlannedState(
    new Planned(rule = rule,
                auctioneerId = auctioneerId,
                auctionId = auctionId))

  override def preStart(): Unit = log.info("Auction started")

  override def postStop(): Unit = log.info("Auction stopped")

  override def receive: Receive = partialUpdateState.andThen(receiveMsg)

  /* This enables us not to create a timer to update the state.
   * Assuming all information from the auction comes through this actor,
   * all the answers require a message, and thus
   * the state will always be updated before answering something.
   */
  private def partialUpdateState: PartialFunction[Any, Any] = {
    case msg => {
      updateState()
      msg
    }
  }

  private def receiveMsg: Receive = {
    case PlannedMessage(plannedMessage) => {
      state match {
        case PlannedState(planned) =>
          val former = planned.rule.copy()
          val answer = planned.receive(plannedMessage)
          if (Planned.datesUpdatedInAnswer(former, answer)) {
            /* The state may have changed, send the message to update
             * it before answering
             */
            self ! GetMessageFrom(sender())
          } else {
            sender() ! answer
          }
        case OpennedState(_) | ClosedState(_) =>
          sender() ! messageNotSupportedAnswer
      }
    }

    case OpennedMessage(opennedMessage) => {
      state match {
        case OpennedState(openned) => {
          sender() ! openned.receive(opennedMessage)
        }
        case PlannedState(_) | ClosedState(_) =>
          sender() ! messageNotSupportedAnswer
      }
    }

    case GetMessage => {
      answerGetMessage(sender())
    }

    case GetMessageFrom(respondTo: ActorRef) => {
      answerGetMessage(respondTo)
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
          sender() ! answerGetBidsOfBidder(bidder,
                                           openned.bidders.toSeq,
                                           openned.bids,
                                           state = "openned")
        }
        case ClosedState(closed) => {
          sender() ! answerGetBidsOfBidder(bidder,
                                           closed.bidders,
                                           closed.bids,
                                           state = "closed")
        }
      }
    }
  }

  private def answerGetMessage(respondTo: ActorRef) = {
    state match {
      case PlannedState(planned) => {
        respondTo ! Answer(StatusCodes.OK, Left(Planned.toPlannedInfo(planned)))
      }
      case OpennedState(openned) => {
        respondTo ! Answer(StatusCodes.OK, Left(Openned.toOpennedInfo(openned)))
      }
      case ClosedState(closed) => {
        respondTo ! Answer(StatusCodes.OK, Left(Closed.toClosedInfo(closed)))
      }
    }
  }

  private def messageNotSupportedAnswer =
    Answer(StatusCodes.BadRequest,
           Right(s"Message not supported in current state $state"))

  private def updateState(): Unit = {
    val currentTime = getCurrentTime()
    state match {
      case PlannedState(notStarted) => {
        if (didEnd(currentTime, notStarted.rule)) {
          state = ClosedState(new Closed(new Openned(notStarted)))
        } else if (didStart(currentTime, notStarted.rule)) {
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
      bidder: Bidder,
      bidders: Seq[AuctionTypes.Bidder],
      bids: List[Bid],
      state: String): GetBidsOfBidderRequestAnswer = {
    val res = bidders.contains(bidder) match {
      case false =>
        Right(
          NotFound(bidder = bidder,
                   auctionId = auctionId,
                   auctioneerId = auctioneerId))
      case true => {
        Left(
          BidsOfBidderInOneAuction(bidder = bidder,
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
    currentTime >= rule.endDate.epochInMilliSec
  }

  private def didStart(currentTime: Long, rule: AuctionRule): Boolean = {
    currentTime >= rule.startDate.epochInMilliSec
  }

}
