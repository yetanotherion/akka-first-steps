package com.spideo.hiring.ion.auction

import java.text.ParseException

import scala.util.{Failure, Success, Try}

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

  private val dateFormat = new java.text.SimpleDateFormat("yyyy/MM/dd-HH:mm")

  def toAuctionDate(date: String): AuctionDate = {
    try {
      val res = dateFormat.parse(date)
      val epoch = res.getTime()
      epoch / 1000
    } catch {
      case e: ParseException => throw new IllegalArgumentException(s"$date is an invalid date")
    }
  }

  def tryToAuctionDate(date: String): Either[AuctionDate, String] = {
    try {
      Left(toAuctionDate(date))
    } catch {
      case e: IllegalArgumentException => Right(e.getMessage())
    }
  }

  def toIncrement(param: Integer): Increment = {
    Constant(param)
  }
}
