package com.spideo.hiring.ion.actors

import akka.actor.{Actor, ActorLogging, Props}
import akka.http.scaladsl.model.StatusCodes
import com.spideo.hiring.ion.auction.AuctionTypes.{AuctionDate, AuctionRule, AuctionRuleAnswer, Error}
import com.spideo.hiring.ion.auction.{Closed, Openned, Planned}
sealed trait State

final case class PlannedState(notStarted: Planned) extends State

final case class OpennedState(onGoingAuction: Openned) extends State

final case class ClosedState(endedAuction: Closed) extends State

object Auction {
  def props(rule: AuctionRule): Props = Props(new Auction(rule))

  final case class PlannedMessage(plannedMessage: Planned.PlannedMessage)
  final case class OpennedMessage(opennedMessage: Openned.Message)

  final object GetMessage

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
          val res = planned.receive(plannedMessage)
          sender() ! res
        case OpennedState(_) | ClosedState(_) => sender() ! messageNotSupportedAnswer
      }
    }
    case OpennedMessage(opennedMessage) => {
      state match {
        case OpennedState(openned) => {
          val res = openned.receive(opennedMessage)
          sender() ! Answer(res)
        }
        case PlannedState(_) | ClosedState(_) => sender() ! messageNotSupportedAnswer
      }
    }
    case GetMessage => {
      state match {
        case PlannedState(planned) => {
          sender() ! AuctionRuleAnswer(StatusCodes.OK, Left(planned.rule))
        }
        case OpennedState(_) | ClosedState(_) => sender() ! messageNotSupportedAnswer
      }
    }
  }

  def messageNotSupportedAnswer = AuctionRuleAnswer(StatusCodes.BadRequest, Right(s"Message not supported in current state $state"))

  private def updateState(): Unit = {
    val currentTime = getCurrentTime()
    state match {
      case PlannedState(notStarted) => {
        if (isEnded(currentTime, notStarted.rule)) {
          state = ClosedState(new Closed(new Openned(notStarted)))
        }
        else if (isStarted(currentTime, notStarted.rule)) {
          state = OpennedState(new Openned(notStarted))
        }
      }
      case OpennedState(onGoing) => {
        if (isEnded(currentTime, onGoing.rule)) {
          state = ClosedState(new Closed(onGoing))
        }
      }
      case ClosedState(_) => ()
    }
  }

  private def isEnded(currentTime: Long, rule: AuctionRule): Boolean = {
    currentTime >= rule.endDate.epochInSec
  }

  private def isStarted(currentTime: Long, rule: AuctionRule): Boolean = {
    currentTime >= rule.startDate.epochInSec
  }

}



