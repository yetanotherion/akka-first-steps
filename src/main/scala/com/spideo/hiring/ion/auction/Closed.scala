package com.spideo.hiring.ion.auction

class Closed(onGoingAuction: Openned) {
  val rule = onGoingAuction.rule
  val bidders= onGoingAuction.bidders
  val currentPrice = onGoingAuction.currentPrice
  val bids = onGoingAuction.bids.toList
}


