package com.spideo.hiring.ion.auction

import com.spideo.hiring.ion.auction.AuctionTypes.AuctionInfo

object Closed {
  def toClosedInfo(closed: Closed): AuctionInfo = {
    AuctionInfo(rule = closed.rule,
      state = "closed",
      bidders = closed.bidders,
      bids = closed.bids,
      winner = closed.winner,
      currentPrice = None,
    )
  }
}

class Closed(onGoingAuction: Openned) {
  val rule = onGoingAuction.rule
  val bidders= onGoingAuction.bidders.toList
  val currentPrice = onGoingAuction.currentPrice
  val bids = onGoingAuction.bids.toList
  val winner = bids match {
    case List() => None
    case hd :: tl => Some(hd)
  }

}


