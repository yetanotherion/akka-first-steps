package com.ion.trials.akka.auction

import akka.http.scaladsl.model.StatusCodes
import com.ion.trials.akka.auction.AuctionTypes._
import com.ion.trials.akka.routes.{AuctionRuleParams, AuctionRuleParamsUpdate}

object Planned {
  type PlannedMessage = AuctionRuleParamsUpdate

  sealed abstract class RawUpdate
  final case class NewRawStartDate(newStartDate: String) extends RawUpdate
  final case class NewRawEndDate(newEndDate: String) extends RawUpdate
  final case class NewRawInitialPrice(newInitialPrice: Price) extends RawUpdate
  final case class NewRawIncrement(newIncrement: Increment) extends RawUpdate
  final case class NewRawItem(newItem: Item) extends RawUpdate

  sealed abstract class Update

  final case class NewStartDate(newStartDate: AuctionDate) extends Update
  final case class NewEndDate(newEndDate: AuctionDate) extends Update
  final case class NewInitialPrice(newInitialPrice: Price) extends Update
  final case class NewIncrement(newIncrement: Increment) extends Update
  final case class NewItem(newItem: Item) extends Update

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
  type PlannedMessageAnswer = Answer[AuctionInfo]

  def toPlannedInfo(planned: Planned): AuctionInfo = {
    AuctionInfo(rule=planned.rule,
      state="planned",
      bidders=List(),
      bids=List(),
      winner=None,
      currentPrice=None,
      auctionId=planned.auctionId,
      auctioneerId=planned.auctioneerId
    )
  }

  def validateAuctionRuleParams(rule: AuctionRuleParams) = {
    val paramsUpdate = new AuctionRuleParamsUpdate(
      startDate=Some(rule.startDate),
      endDate=Some(rule.endDate),
      initialPrice =Some(rule.initialPrice),
      item=Some(rule.item),
      increment=Some(rule.increment))
    updateAuctionRule(paramsUpdate, initialRule)
  }

  private def updateAuctionRule(paramsUpdate: AuctionRuleParamsUpdate, rule: AuctionRule): Either[AuctionRule, List[String]] = {
    val instructions = createUpdateInstructions(paramsUpdate)
    validateRawUpdates(instructions, rule) match {
      case Left(updates) => {
        val res = implementUpdates(updates, rule)
        Left(res)
      }
      case Right(error) => Right(error)
    }
  }

  private def createUpdateInstructions(message: PlannedMessage) : List[RawUpdate] = {
    def doUpdate[T](l: List[RawUpdate], get: () => Option[T], toUpdate: T => RawUpdate): List[RawUpdate] = {
      get() match {
        case None => l
        case Some(x) => toUpdate(x) :: l
      }
    }

    val updateStart = { l: List[RawUpdate] =>
      doUpdate(l, () => message.startDate, NewRawStartDate.apply)
    }

    val updateEnd = { l: List[RawUpdate] =>
      doUpdate(l, () => message.endDate, NewRawEndDate.apply)
    }

    val updateNewPrice = { l: List[RawUpdate] =>
      doUpdate(l, () => message.initialPrice, NewRawInitialPrice.apply)
    }

    val updateNewIncrement = { l: List[RawUpdate] =>
      doUpdate(l, () => message.increment, (x: Int) => NewRawIncrement.apply(toIncrement(x)))
    }

    val updateNewItem = { l: List[RawUpdate] =>
      doUpdate(l, () => message.item, NewRawItem.apply)
    }

    List(updateStart, updateEnd, updateNewPrice,
      updateNewIncrement, updateNewItem)
      .foldLeft(List[RawUpdate]()) { (l, f) => f(l)
    }
  }

  private def validateRawUpdates(updates: List[RawUpdate], rule: AuctionRule): UpdateOrError = {
    def validateRawUpdate(res: UpdateOrError, update: RawUpdate): UpdateOrError = {
      update match {
        case NewRawStartDate(newStartDate) => {
          tryToAuctionDate(newStartDate) match {
            case Left(ok) => validateNewStartDate(ok, rule) match {
              case None => addMessage(res, NewStartDate(ok))
              case Some(error) => addError(res, error)
            }
            case Right(error) => addError(res, error)
          }
        }
        case NewRawEndDate(newEndDate) => {
          tryToAuctionDate(newEndDate) match {
            case Left(ok) => validateNewEndDate(ok ,rule) match {
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
        case NewRawIncrement(newIncrement) => addMessage(res, NewIncrement(newIncrement))
        case NewRawItem(newRawItem) => addMessage(res, NewItem(newRawItem))
      }
    }
    updates.foldLeft(emptyUpdateOrError)(validateRawUpdate)
  }

  private def implementUpdates(updates: List[Update], rule:AuctionRule): AuctionRule = {
    def implementUpdate(res: AuctionRule, update: Update): AuctionRule = {
      update match {
        case NewStartDate(newStartDate) => res.copy(startDate = newStartDate)
        case NewEndDate(newEndDate) => res.copy(endDate = newEndDate)
        case NewInitialPrice(newInitialPrice) => res.copy(initialPrice = newInitialPrice)
        case NewIncrement(newIncrement) => res.copy(increment = newIncrement)
        case NewItem(newItem) => res.copy(item = newItem)
      }
    }
    updates.foldLeft(rule)(implementUpdate)
  }

  private def validateNewStartDate(newStartDate: AuctionDate, rule: AuctionRule): Option[String] = {
    if (newStartDate.epochInSec > rule.endDate.epochInSec) {
      Some(s"newStartDate: ${fromAuctionDate(newStartDate)} > endDate: ${fromAuctionDate(rule.endDate)}")
    } else {
      None
    }
  }

  private def validateNewEndDate(newEndDate: AuctionDate, rule: AuctionRule): Option[String] = {
    if (newEndDate.epochInSec < rule.startDate.epochInSec) {
      Some(s"newEndDate: ${fromAuctionDate(newEndDate)} < startDate: ${fromAuctionDate(rule.startDate)}")
    } else {
      None
    }
  }

  private def validateNewInitialPrice(newInitialPrice: Price): Option[String] = {
    if (newInitialPrice < 0) {
      Some(s"$newInitialPrice is negative")
    } else {
      None
    }
  }

  private val initialRule = new AuctionRule(startDate=AuctionDate(0),
    endDate=AuctionDate(Long.MaxValue),
    initialPrice=0, increment=Increment(0), item=0)

}

class Planned(var rule: AuctionRule, val auctionId: AuctionId, val auctioneerId: AuctioneerId) {

  import Planned._

  def receive(message: PlannedMessage): PlannedMessageAnswer = {
    updateAuctionRule(message, rule) match {
      case Left(newRule) => {
        rule = newRule
        Answer(StatusCodes.OK, Left(toPlannedInfo(this)))
      }
      case Right(error) => Answer(StatusCodes.BadRequest, Right(s"invalid request: ${error.mkString(";")}"))
    }
  }

}
