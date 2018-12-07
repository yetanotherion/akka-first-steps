package com.spideo.hiring.ion.auction

import java.text.ParseException

import akka.http.scaladsl.model.StatusCode

import scala.util.{Failure, Success, Try}

object AuctionTypes {
  type AuctionDate = Long // epoch
  type Item = Int
  type Price = Int
  type Bidder = Int
  type AuctionId = Int
  type AuctioneerId = Int
  final case class Increment(value: Price)
  case class Error(msg:String)

  final case class Bid(bidder: Bidder, price: Price)

  final case class AuctionRule(
    var startDate: AuctionDate, var endDate: AuctionDate,
    var item: Item, var initialPrice: Price, var increment: Increment)

  final case class CreateAuctionAnswer(status: StatusCode, msg: String)

  final case class UpdateAuctionAnswer(status: StatusCode, msg: Either[AuctionRule, String])

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
    Increment(param)
  }
}
