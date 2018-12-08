package com.spideo.hiring.ion.actors

import akka.actor.{Actor, ActorLogging, Props}
import akka.http.scaladsl.model.StatusCodes
import com.spideo.hiring.ion.auction.AuctionTypes.{AuctionAnswer, AuctionRule}
import com.spideo.hiring.ion.auction.{Closed, Openned, Planned}
sealed trait State

final case class PlannedState(notStarted: Planned) extends State

final case class OpennedState(onGoingAuction: Openned) extends State

final case class ClosedState(endedAuction: Closed) extends State

object Auction {
  def props(rule: AuctionRule): Props = Props(new Auction(rule))

  sealed abstract class Message
  final case class PlannedMessage(plannedMessage: Planned.PlannedMessage) extends Message
  final case class OpennedMessage(opennedMessage: Openned.Message) extends Message
  final object GetMessage extends Message

  final case class Answer(error: Option[Error])
  def getCurrentTime(): Long = {
    System.currentTimeMillis / 1000
  }
}

class Auction(rule: AuctionRule) extends Actor with ActorLogging {
  import Auction._
  private var state: State = PlannedState(new Planned(rule))

  override def preStart(): Unit = log.info("Auction started")
  override def postStop(): Unit = log.info("Auction stopped")

  override def receive: Receive = partialUpdateState.andThen(receiveMsg)

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
          sender() ! AuctionAnswer(StatusCodes.OK, Left(Planned.toPlannedInfo(planned.rule)))
        }
        case OpennedState(openned) => {
          sender() ! AuctionAnswer(StatusCodes.OK, Left(Openned.toOpennedInfo(openned)))
        }
        case ClosedState(closed) => {
          sender() ! AuctionAnswer(StatusCodes.OK, Left(Closed.toClosedInfo(closed)))
        }
      }
    }
  }

  def messageNotSupportedAnswer = AuctionAnswer(StatusCodes.BadRequest, Right(s"Message not supported in current state $state"))

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

  private def didEnd(currentTime: Long, rule: AuctionRule): Boolean = {
    currentTime >= rule.endDate.epochInSec
  }

  private def didStart(currentTime: Long, rule: AuctionRule): Boolean = {
    currentTime >= rule.startDate.epochInSec
  }

}



