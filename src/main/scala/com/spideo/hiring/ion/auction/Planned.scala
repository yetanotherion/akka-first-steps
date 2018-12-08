package com.spideo.hiring.ion.auction

import akka.http.scaladsl.model.StatusCodes
import com.spideo.hiring.ion.auction.AuctionTypes._
import com.spideo.hiring.ion.routes.AuctionRuleParamsUpdate

object Planned {
  type PlannedMessage = AuctionRuleParamsUpdate

  sealed abstract class RawUpdate
  final case class NewRawStartDate(newStartDate: String) extends RawUpdate
  final case class NewRawEndDate(newEndDate: String) extends RawUpdate
  final case class NewRawInitialPrice(newInitialPrice: Price) extends RawUpdate

  sealed abstract class Update

  final case class NewStartDate(newStartDate: AuctionDate) extends Update
  final case class NewEndDate(newEndDate: AuctionDate) extends Update
  final case class NewInitialPrice(newInitialPrice: Price) extends Update

  type UpdateOrError = Either[List[Update], List[String]]

  val emptyUpdateOrError: UpdateOrError = Left(List())

  def addMessage(res: UpdateOrError, message: Update) = {
    res match {
      case Left(ok) => Left(message :: ok)
      case Right(errors) => Right(errors)
    }
  }

  def addError(res: UpdateOrError, error: String) = {
    res match {
      case Left(ok) => Right(List(error))
      case Right(errors) => Right(error :: errors)
    }
  }
  type PlannedMessageAnswer = AuctionRuleAnswer
}

class Planned(val rule: AuctionRule) {
  import Planned._

  def receive(message: PlannedMessage): PlannedMessageAnswer = {
    val instructions = createUpdateInstructions(message)
    validateRawUpdates(instructions) match {
      case Left(updates) => {
        implementUpdates(updates)
        AuctionRuleAnswer(StatusCodes.OK, Left(rule))
      }
      case Right(error) => AuctionRuleAnswer(StatusCodes.BadRequest, Right(s"invalid request: ${error.mkString(";")}"))
    }
  }

  private def createUpdateInstructions(message: PlannedMessage) : List[RawUpdate] = {
    val updateStart: List[RawUpdate] => List[RawUpdate] = { l =>
      message.startDate match {
        case None => l
        case Some(startDate) => {
          NewRawStartDate(startDate) :: l
        }
      }
    }
    val updateEnd: List[RawUpdate] => List[RawUpdate] = { l =>
      message.endDate match {
        case None => l
        case Some(endDate) => {
          NewRawEndDate(endDate) :: l
        }
      }
    }
    val updateNewPrice: List[RawUpdate] => List[RawUpdate] = { l =>
      message.initialPrice match {
        case None => l
        case Some(initialPrice) => {
          NewRawInitialPrice(initialPrice) :: l
        }
      }
    }
    List(updateStart, updateEnd, updateNewPrice).foldLeft(List[RawUpdate]()) {
      (l, f) => f(l)
    }
  }

  private def validateRawUpdates(updates: List[RawUpdate]): UpdateOrError = {
    def validateRawUpdate(res: UpdateOrError, update: RawUpdate): UpdateOrError = {
      update match {
        case NewRawStartDate(newStartDate) => {
          tryToAuctionDate(newStartDate) match {
            case Left(ok) => validateNewStartDate(ok) match {
              case None => addMessage(res, NewStartDate(ok))
              case Some(error) => addError(res, error)
            }
            case Right(error) => addError(res, error)
          }
        }
        case NewRawEndDate(newEndDate) => {
          tryToAuctionDate(newEndDate) match {
            case Left(ok) => validateNewEndDate(ok) match {
              case None => addMessage(res, NewEndDate(ok))
              case Some(error) => addError(res, error)
            }
            case Right(error) => addError(res, error)
          }
        }
        case NewRawInitialPrice(newInitialPrice) => {
          validateNewInitialPrice(newInitialPrice) match {
            case None => addMessage(res, NewInitialPrice(newInitialPrice))
            case Some(error) => addError(res, error)
          }
        }
      }
    }
    updates.foldLeft(emptyUpdateOrError)(validateRawUpdate)
  }

  private def implementUpdates(updates: List[Update]): Unit = {
    def implementUpdate(update: Update): Unit = {
      update match {
        case NewStartDate(newStartDate) => rule.startDate = newStartDate
        case NewEndDate(newEndDate) => rule.endDate = newEndDate
        case NewInitialPrice(newInitialPrice) => rule.initialPrice = newInitialPrice
      }
    }
    updates.foreach(implementUpdate)
  }

  private def validateNewStartDate(newStartDate: AuctionDate): Option[String] = {
    if (newStartDate.epochInSec > rule.endDate.epochInSec) {
      Some(s"newStartDate: ${fromAuctionDate(newStartDate)} > endDate: ${fromAuctionDate(rule.endDate)}")
    } else {
      None
    }
  }

  private def validateNewEndDate(newEndDate: AuctionDate): Option[String] = {
    if (newEndDate.epochInSec < rule.startDate.epochInSec) {
      Some(s"newEndDate: ${fromAuctionDate(newEndDate)} < startDate: ${fromAuctionDate(rule.startDate)}")
    } else {
      None
    }
  }


  private def validateNewInitialPrice(newInitialPrice: Price): Option[String] = {
    if (newInitialPrice < 0 ) {
      Some(s"$newInitialPrice is negative")
    } else {
      None
    }
  }

}
