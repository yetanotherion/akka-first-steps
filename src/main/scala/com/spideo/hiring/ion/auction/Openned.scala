package com.spideo.hiring.ion.auction

import akka.http.scaladsl.model.StatusCodes
import com.spideo.hiring.ion.auction.AuctionTypes._

import scala.collection.mutable.ListBuffer

object Openned {

  sealed abstract class Message

  final case class NewBidder(newBidder: Bidder) extends Message

  final case class NewBid(newBid: Bid) extends Message

  def toOpennedInfo(openned: Openned): AuctionInfo = {
    AuctionInfo(rule = openned.rule,
      state = "open",
      bidders = openned.bidders.toList,
      bids = openned.bids.toList,
      winner = None,
      currentPrice = Some(openned.currentPrice),
    )
  }
}

class Openned(notStarted: Planned)
{

  import Openned._

  var bids = ListBuffer[Bid]()
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
      case Some(error) => AuctionAnswer(StatusCodes.BadRequest, Right(error.msg))
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

