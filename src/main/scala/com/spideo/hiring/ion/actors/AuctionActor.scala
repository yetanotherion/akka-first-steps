package com.spideo.hiring.ion.actors

import akka.actor.{Actor, ActorLogging, Props}
import com.spideo.hiring.ion.auction.AuctionTypes.{AuctionRule, Error}
import com.spideo.hiring.ion.auction.{Closed, Openned, Planned}
sealed trait State

final case class PlannedState(notStarted: Planned) extends State

final case class OpennedState(onGoingAuction: Openned) extends State

final case class ClosedState(endedAuction: Closed) extends State

object Auction {
  def props(rule: AuctionRule): Props = Props(new Auction(rule))

  final case class PlannedMessage(plannedMessage: Planned.PlannedMessage)
  final case class OpennedMessage(opennedMessage: Openned.Message)

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
  }

  def messageNotSupportedAnswer = Answer(Some(Error("Message not supported in current state")))

  private def updateState(): Unit = {
    val currentTime = getCurrentTime()
    state match {
      case PlannedState(notStarted) => {
        if (notStarted.rule.startDate >= currentTime) {
          state = OpennedState(new Openned(notStarted))
        }
      }
      case OpennedState(onGoing) => {
        if (onGoing.rule.endDate >= currentTime) {
          state = ClosedState(new Closed(onGoing))
        }
      }
      case ClosedState(_) => ()
    }
  }

}



