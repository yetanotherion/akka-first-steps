package com.spideo.hiring.ion.auction

import com.spideo.hiring.ion.Bid
import com.spideo.hiring.ion.auction.AuctionTypes.{Constant, Error}

import scala.collection.mutable.ListBuffer

object Openned {
  sealed abstract class Message
  final case class NewBid(newBid: Bid) extends Message
}

class Openned(notStarted: Planned)
{
  import Openned._

  var bids = ListBuffer[Bid]()
  var currentPrice = notStarted.rule.initialPrice
  val rule = notStarted.rule
  val bidders = notStarted.getBidders()

  def receive(message: Message): Option[Error] = {
    message match {
      case NewBid(bid) => receiveBid(bid)
    }
  }

  private def receiveBid(bid: Bid): Option[Error] = {
    validateBid(bid) match {
      case Some(error) => Some(error)
      case None => addBid(bid)
    }
  }

  private def validateBid(bid: Bid): Option[Error] = {
    val bidder = bid.bidder
    if (!bidders.contains(bidder)) {
      return Some(Error(s"${bidder} did not join the auction yet"))
    }
    if (!validateIncrement(bid)) {
      return Some(Error(s"${bid.price} does not respect increment rules"))
    }
    None
  }

  private def validateIncrement(bid: Bid): Boolean = {
    rule.increment match {
      case Constant(value) => value <= bid.price - currentPrice
    }
  }

  private def addBid(bid: Bid): Option[Error] = {
    currentPrice = bid.price
    bids += bid
    None
  }
}

