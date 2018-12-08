package com.spideo.hiring.ion.auction

import java.text.ParseException
import java.time.{Instant, LocalDate, LocalDateTime, ZoneId, ZonedDateTime}
import java.time.format.{DateTimeFormatter, DateTimeParseException}

import akka.http.scaladsl.model.StatusCode

import scala.util.{Failure, Success, Try}

object AuctionTypes {
  final case class AuctionDate(epochInSec: Long)

  type Item = Int
  type Price = Int
  type Bidder = Int
  type AuctionId = Int
  type AuctioneerId = Int
  final case class Increment(value: Price)

  final case class BidParam(bid: Price)
  final case class Bid(bidder: Bidder, price: Price)

  final case class AuctionRule(
    var startDate: AuctionDate, var endDate: AuctionDate,
    var item: Item, var initialPrice: Price, var increment: Increment)

  final case class AuctionInfo(
    rule: AuctionRule,
    state: String,
    bidders: List[Bidder],
    bids: List[Bid],
    winner: Option[Bid],
    currentPrice: Option[Price]
  )
  final case class AuctionAnswer(status: StatusCode, msg: Either[AuctionInfo, String])

  private val dateFormat = DateTimeFormatter.ISO_OFFSET_DATE_TIME
  private val zoneId  = ZoneId.systemDefault()

  def toAuctionDate(date: String): AuctionDate = {
    try {
      val localDate = LocalDateTime.parse(date, dateFormat)
      AuctionDate(localDate.atZone(zoneId).toEpochSecond)
    } catch {
      case e: DateTimeParseException => throw new IllegalArgumentException(s"$date is an invalid date")
    }
  }

  def fromAuctionDate(auctionDate: AuctionDate): String = {
    ZonedDateTime.ofInstant(Instant.ofEpochSecond(auctionDate.epochInSec), zoneId).format(dateFormat)
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
