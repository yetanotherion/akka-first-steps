package com.spideo.hiring.ion.auction

import akka.actor.{Actor, ActorLogging, Props}
import com.spideo.hiring.ion.auction.AuctionTypes.Error
sealed trait State

final case class PlannedState(notStarted: Planned) extends State

final case class OpennedState(onGoingAuction: Openned) extends State

final case class ClosedState(endedAuction: Closed) extends State

object Auction {
  def props(rule: AuctionRule): Props = Props(new Auction(rule))

  final case class PlannedMessage(plannedMessage: Planned.Message)
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

  override def receive: Receive = {
    case PlannedMessage(plannedMessage) => {
      updateState()
      state match {
        case PlannedState(planned) =>
          val res = planned.receive(plannedMessage)
          sender() ! Answer(res)
        case OpennedState(_) | ClosedState(_) => sender() ! messageNotSupportedAnswer
      }
    }
    case OpennedMessage(opennedMessage) => {
      updateState()
      state match {
        case OpennedState(openned) => {
          val res = openned.receive(opennedMessage)
          sender() ! Answer(res)
        }
        case (PlannedState(_) | ClosedState(_)) => sender() != messageNotSupportedAnswer
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



