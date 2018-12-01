package com.spideo.hiring.ion.auction

object AuctionTypes {
  type AuctionDate = Long // epoch
  type Item = Int
  type Price = Int
  type Bidder = Int
  type AuctionId = Int
  type AuctioneerId = Int
  sealed abstract class Increment
  case class Constant(value: Price) extends Increment
  case class Error(msg:String)
  final case class Bid(bidder: Bidder, price: Price)
}
