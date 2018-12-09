package com.ion.trials.akka.auction

import AuctionTypes.AuctionInfo

object Closed {
  def toClosedInfo(closed: Closed): AuctionInfo = {
    AuctionInfo(
      rule = closed.rule,
      state = "closed",
      bidders = closed.bidders,
      bids = closed.bids,
      winner = closed.winner,
      currentPrice = None,
      auctionId = closed.auctionId,
      auctioneerId = closed.auctioneerId
    )
  }
}

class Closed(openned: Openned) {
  val rule = openned.rule
  val auctionId = openned.auctionId
  val auctioneerId = openned.auctioneerId
  val bidders = openned.bidders.toList
  val currentPrice = openned.currentPrice
  val bids = openned.bids
  val winner = bids match {
    case List()   => None
    case hd :: tl => Some(hd)
  }

}
