package com.spideo.hiring.ion.auction

import com.spideo.hiring.ion.auction.AuctionTypes.{Bid, Bidder, Error, Increment}

import scala.collection.mutable.ListBuffer

object Openned {
  sealed abstract class Message
  final case class NewBidder(newBidder: Bidder) extends Message
  final case class NewBid(newBid: Bid) extends Message
}

class Openned(notStarted: Planned)
{
  import Openned._

  var bids = ListBuffer[Bid]()
  var currentPrice = notStarted.rule.initialPrice
  val rule = notStarted.rule
  val bidders = scala.collection.mutable.Set[Bidder]()

  def receive(message: Message): Option[Error] = {
    message match {
      case NewBid(bid) => receiveBid(bid)
      case NewBidder(bidder) => receiveBidder(bidder)
    }
  }

  private def receiveBid(bid: Bid): Option[Error] = {
    validateBid(bid) match {
      case Some(error) => Some(error)
      case None => addBid(bid)
    }
  }

  private def receiveBidder(bidder: AuctionTypes.Bidder): Option[Error] = {
    bidders.add(bidder)
    None
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
      case Increment(value) => value <= bid.price - currentPrice
    }
  }

  private def addBid(bid: Bid): Option[Error] = {
    currentPrice = bid.price
    bids += bid
    None
  }
}

