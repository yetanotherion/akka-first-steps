package com.spideo.hiring.ion.auction

import com.spideo.hiring.ion.auction.AuctionTypes._

object Planned {
  sealed abstract class Message
  final case class NewStartDate(newStartDate: AuctionDate) extends Message
  final case class NewEndDate(newStartDate: AuctionDate) extends Message
}

final case class AuctionRule(
  var startDate: AuctionDate, var endDate: AuctionDate,
  var item: Item, var initialPrice: Price, var increment: Increment)

class Planned(val rule: AuctionRule) {
  import Planned._
  private var bidders = scala.collection.mutable.Set[Bidder]()

  def getBidders(): Set[Bidder] = {
    bidders.toSet[Bidder]
  }

  def receive(message: Message): Option[Error] = {
    message match {
      case NewStartDate(startDate) => receiveNewStartDate(startDate)
      case NewEndDate(endDate) => receiveNewEndDate(endDate)
    }
  }

  private def receiveNewBidder(bidder: Bidder) = {
    bidders.add(bidder)
  }

  private def receiveNewStartDate(newStartDate: AuctionDate): Option[Error] = {
    if (newStartDate > rule.endDate) {
      Some(Error(s"newStartDate: $newStartDate > endDate: ${rule.endDate}"))
    }
    rule.startDate = newStartDate
    None
  }

  private def receiveNewEndDate(newEndDate: AuctionDate): Option[Error] = {
    if (newEndDate < rule.startDate) {
      Some(Error(s"newEndDate: $newEndDate < startDate: ${rule.startDate}"))
    }
    rule.endDate = newEndDate
    None
  }


  private def receiveNewInitialPrice(newInitialPrice: Price): Option[Error] = {
    if (newInitialPrice < 0 ) {
      Some(Error(s"$newInitialPrice is negative"))
    }
    rule.initialPrice = newInitialPrice
    None
  }

  private def receiveNewIncrement(newIncrement: Increment): Unit  = {
    rule.increment = newIncrement
  }
}
