package com.spideo.hiring.ion.auction

import akka.http.scaladsl.model.StatusCodes
import com.spideo.hiring.ion.auction.AuctionTypes._

object Openned {

  sealed abstract class Message

  final case class NewBidder(newBidder: Bidder) extends Message

  final case class NewBid(newBid: Bid) extends Message

  def toOpennedInfo(openned: Openned): AuctionInfo = {
    AuctionInfo(rule = openned.rule,
      state = "open",
      bidders = openned.bidders.toList,
      bids = openned.bids,
      winner = None,
      currentPrice = Some(openned.currentPrice),
    )
  }
}

class Openned(notStarted: Planned)
{

  import Openned._

  var bids = List[Bid]()
  var currentPrice = notStarted.rule.initialPrice
  val rule = notStarted.rule
  val bidders = scala.collection.mutable.Set[Bidder]()

  def receive(message: Message): AuctionAnswer = {
    message match {
      case NewBid(bid) => receiveBid(bid)
      case NewBidder(bidder) => receiveBidder(bidder)
    }
  }

  private def receiveBid(bid: Bid): AuctionAnswer = {
    validateBid(bid) match {
      case Some(error) => AuctionAnswer(StatusCodes.BadRequest, Right(error))
      case None => {
        addBid(bid)
        AuctionAnswer(StatusCodes.OK, Left(toOpennedInfo(this)))
      }
    }
  }

  private def receiveBidder(bidder: AuctionTypes.Bidder): AuctionAnswer =
  {
    bidders.add(bidder)
    AuctionAnswer(StatusCodes.OK, Left(toOpennedInfo(this)))
  }

  private def validateBid(bid: Bid): Option[String] = {
    val bidder = bid.bidder
    if (!bidders.contains(bidder)) {
      return Some(s"${bidder} did not join the auction yet")
    }
    if (!validateIncrement(bid)) {
      return Some(s"${bid.price} does not respect increment rules")
    }
    None
  }

  private def validateIncrement(bid: Bid): Boolean = {
    rule.increment match {
      case Increment(value) => value <= bid.price - currentPrice
    }
  }

  private def addBid(bid: Bid): Unit = {
    currentPrice = bid.price
    bids = bid :: bids
  }
}

