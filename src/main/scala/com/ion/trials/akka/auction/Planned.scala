package com.ion.trials.akka.auction

import akka.http.scaladsl.model.StatusCodes
import com.ion.trials.akka.auction.AuctionTypes._
import com.ion.trials.akka.service.AuctionService.{
  AuctionRuleParams,
  AuctionRuleParamsUpdate,
  IncrementParams
}

object Planned {
  type PlannedMessage = AuctionRuleParamsUpdate

  sealed abstract class RawUpdate
  final case class NewRawStartDate(newStartDate: String) extends RawUpdate
  final case class NewRawEndDate(newEndDate: String) extends RawUpdate
  final case class NewRawInitialPrice(newInitialPrice: Price) extends RawUpdate
  final case class NewRawIncrement(newIncrement: IncrementParams)
      extends RawUpdate
  final case class NewRawItem(newItem: Item) extends RawUpdate

  sealed abstract class Update

  final case class NewStartDate(newStartDate: AuctionDate) extends Update
  final case class NewEndDate(newEndDate: AuctionDate) extends Update
  final case class NewInitialPrice(newInitialPrice: Price) extends Update
  final case class NewIncrement(newIncrement: Increment) extends Update
  final case class NewItem(newItem: Item) extends Update

  type UpdateOrError = Either[List[Update], List[String]]

  type PlannedMessageAnswer = Answer[AuctionInfo]

  val plannedStr = "planned"

  def toPlannedInfo(planned: Planned): AuctionInfo = {
    AuctionInfo(
      rule = planned.rule,
      state = plannedStr,
      bidders = List(),
      bids = List(),
      winner = None,
      currentPrice = None,
      auctionId = planned.auctionId,
      auctioneerId = planned.auctioneerId
    )
  }

  def datesUpdatedInAnswer(former: AuctionRule, answer: Answer[AuctionInfo]) = {
    getSuccess(answer) match {
      case Some(x) =>
        if (datesUpdated(former, x.rule)) {
          true
        } else {
          false
        }
      case None => false
    }
  }

  private def datesUpdated(before: AuctionRule, after: AuctionRule) = {
    before.startDate != after.startDate || before.endDate != after.endDate
  }

  def validateAuctionRuleParams(rule: AuctionRuleParams) = {
    val paramsUpdate = new AuctionRuleParamsUpdate(
      startDate = Some(rule.startDate),
      endDate = Some(rule.endDate),
      initialPrice = Some(rule.initialPrice),
      item = Some(rule.item),
      increment = Some(rule.increment))
    updateAuctionRule(paramsUpdate, initialRule)
  }

  private def updateAuctionRule(
      paramsUpdate: AuctionRuleParamsUpdate,
      rule: AuctionRule): Either[AuctionRule, List[String]] = {
    val instructions = createUpdateInstructions(paramsUpdate)
    validateRawUpdates(instructions, rule) match {
      case Left(updates) => {
        val res = implementUpdates(updates, rule)
        Left(res)
      }
      case Right(error) => Right(error)
    }
  }

  private def createUpdateInstructions(
      message: PlannedMessage): List[RawUpdate] = {
    def doUpdate[T](l: List[RawUpdate],
                    get: () => Option[T],
                    toUpdate: T => RawUpdate): List[RawUpdate] = {
      get() match {
        case None    => l
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
      doUpdate(l,
               () => message.increment,
               (x: IncrementParams) => NewRawIncrement.apply(x))
    }

    val updateNewItem = { l: List[RawUpdate] =>
      doUpdate(l, () => message.item, NewRawItem.apply)
    }

    List(updateStart,
         updateEnd,
         updateNewPrice,
         updateNewIncrement,
         updateNewItem)
      .foldLeft(List[RawUpdate]()) { (l, f) =>
        f(l)
      }
  }

  private def validateRawUpdates(updates: List[RawUpdate],
                                 rule: AuctionRule): UpdateOrError = {
    def validateRawUpdate(res: UpdateOrError,
                          update: RawUpdate): UpdateOrError = {
      update match {
        case NewRawStartDate(newStartDate) => {
          tryToAuctionDate(newStartDate) match {
            case Left(ok) =>
              validateNewStartDate(ok, rule) match {
                case None        => addMessage(res, NewStartDate(ok))
                case Some(error) => addError(res, error)
              }
            case Right(error) => addError(res, error)
          }
        }
        case NewRawEndDate(newEndDate) => {
          tryToAuctionDate(newEndDate) match {
            case Left(ok) =>
              validateNewEndDate(ok, rule) match {
                case None        => addMessage(res, NewEndDate(ok))
                case Some(error) => addError(res, error)
              }
            case Right(error) => addError(res, error)
          }
        }
        case NewRawInitialPrice(newInitialPrice) => {
          validateNewInitialPrice(newInitialPrice) match {
            case None        => addMessage(res, NewInitialPrice(newInitialPrice))
            case Some(error) => addError(res, error)
          }
        }
        case NewRawIncrement(newIncrement) =>
          validateNewRawIncrement(newIncrement) match {
            case Left(increment) => addMessage(res, NewIncrement(increment))
            case Right(error)    => addError(res, error)
          }

        case NewRawItem(newRawItem) => addMessage(res, NewItem(newRawItem))
      }
    }
    updates.foldLeft(emptyUpdateOrError)(validateRawUpdate)
  }

  private def implementUpdates(updates: List[Update],
                               rule: AuctionRule): AuctionRule = {
    def implementUpdate(res: AuctionRule, update: Update): AuctionRule = {
      update match {
        case NewStartDate(newStartDate) => res.copy(startDate = newStartDate)
        case NewEndDate(newEndDate)     => res.copy(endDate = newEndDate)
        case NewInitialPrice(newInitialPrice) =>
          res.copy(initialPrice = newInitialPrice)
        case NewIncrement(newIncrement) => res.copy(increment = newIncrement)
        case NewItem(newItem)           => res.copy(item = newItem)
      }
    }
    updates.foldLeft(rule)(implementUpdate)
  }

  private def validateNewStartDate(newStartDate: AuctionDate,
                                   rule: AuctionRule): Option[String] = {
    if (newStartDate.epochInMilliSec > rule.endDate.epochInMilliSec) {
      Some(
        s"newStartDate: ${fromAuctionDate(newStartDate)} > endDate: ${fromAuctionDate(rule.endDate)}")
    } else {
      None
    }
  }

  private def validateNewEndDate(newEndDate: AuctionDate,
                                 rule: AuctionRule): Option[String] = {
    if (newEndDate.epochInMilliSec < rule.startDate.epochInMilliSec) {
      Some(
        s"newEndDate: ${fromAuctionDate(newEndDate)} < startDate: ${fromAuctionDate(rule.startDate)}")
    } else {
      None
    }
  }

  private def validateNewInitialPrice(
      newInitialPrice: Price): Option[String] = {
    if (newInitialPrice < 0) {
      Some(s"$newInitialPrice is negative")
    } else {
      None
    }
  }

  private def validateNewRawIncrement(
      increment: IncrementParams): Either[Increment, String] = {
    val sortedParams = increment.slots.sortBy { _.endOfSlot }
    val maxValues = sortedParams.map { _.endOfSlot }
    val negativeEndOfSlots = maxValues.filter { _ < 0 }
    if (!negativeEndOfSlots.isEmpty) {
      return Right(s"endOfSlots: '${negativeEndOfSlots}' are negative")
    }
    val negativeIncrements =
      sortedParams.map { _.minIncrement }.filter { _ < 0 }
    if (!negativeIncrements.isEmpty) {
      return Right(s"minIncrements: '${negativeIncrements}' are negative")
    }
    val uniqueValue = sortedParams.map { _.endOfSlot }.toSet
    if (uniqueValue.size != sortedParams.size) {
      val duplicated = uniqueValue
        .map { value =>
          (value, maxValues.count { maxVal =>
            maxVal == value
          })
        }
        .filter(_._2 > 1)
        .map(_._1)
      return Right(s"endOfSlots: '${duplicated}' are duplicated")
    }
    val default = increment.minIncrementAfterLastSlot
    val lastSlot = List(
      IncrementSlot(endOfSlot = Integer.MAX_VALUE, minIncrement = default))
    sortedParams.reverse match {
      case last :: tl => {
        if (last.endOfSlot == Integer.MAX_VALUE && default != last.minIncrement) {
          Right(
            s"minIncrementAfterLastSlot value $default and ${last.minIncrement} are incompatible")
        } else {
          Left(Increment(slots = sortedParams ::: lastSlot))
        }
      }
      case List() => Left(Increment(lastSlot))
    }
  }

  private val emptyUpdateOrError: UpdateOrError = Left(List())

  private def addMessage(res: UpdateOrError, message: Update) = {
    res match {
      case Left(ok)      => Left(message :: ok)
      case Right(errors) => Right(errors)
    }
  }

  private def addError(res: UpdateOrError, error: String) = {
    res match {
      case Left(ok)      => Right(List(error))
      case Right(errors) => Right(error :: errors)
    }
  }

  private val initialRule = new AuctionRule(
    startDate = AuctionDate(0),
    endDate = AuctionDate(Long.MaxValue),
    initialPrice = 0,
    increment = Increment(List(IncrementSlot(Integer.MAX_VALUE, 0))),
    item = 0)

}

class Planned(var rule: AuctionRule,
              val auctionId: AuctionId,
              val auctioneerId: AuctioneerId) {

  import Planned._

  def receive(message: PlannedMessage): PlannedMessageAnswer = {
    updateAuctionRule(message, rule) match {
      case Left(newRule) => {
        rule = newRule
        Answer(StatusCodes.OK, Left(toPlannedInfo(this)))
      }
      case Right(error) =>
        Answer(StatusCodes.BadRequest,
               Right(Error(s"invalid request: ${error.mkString(";")}")))
    }
  }

}
