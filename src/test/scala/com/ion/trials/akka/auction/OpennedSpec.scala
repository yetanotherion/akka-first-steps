package com.ion.trials.akka.auction

import akka.http.scaladsl.model.StatusCodes
import com.ion.trials.akka.auction.AuctionTypes.{
  Answer,
  Bid,
  Increment,
  IncrementSlot,
  Price
}
import com.ion.trials.akka.auction.Openned.{NewBid, NewBidder}
import com.ion.trials.akka.util.AuctionTestData
import org.scalacheck.Properties
import org.scalacheck.Gen.posNum
import org.scalacheck.Prop.forAll

object OpennedSpec
    extends Properties("Openned auction tests")
    with AuctionTestData {

  val firstSlot = IncrementSlot(endOfSlot = 10, minIncrement = 2)
  val lastSlot = IncrementSlot(endOfSlot = Integer.MAX_VALUE, minIncrement = 10)

  val openned = new Openned(
    new Planned(
      rule = auctionRule.copy(initialPrice = 10,
                              increment = Increment(List(firstSlot, lastSlot))),
      auctionId = 0,
      auctioneerId = 1
    ))

  val joinedBidders = List(0, 3, 5)

  joinedBidders.foreach { bidder =>
    openned.receive(NewBidder(newBidder = bidder))
  }

  property("Bids lower than current price are always refused") =
    forAll(posNum[Int]) { bid =>
      val notBiggerThanInitialPrice = bid <= openned.currentPrice
      notBiggerThanInitialPrice match {
        case true => {
          openned.receive(NewBid(Bid(bidder = joinedBidders.head, price = bid))) match {
            case Answer(StatusCodes.BadRequest, _) => true
            case _                                 => false
          }
        }
        case false => true
      }
    }

  property("Bids that don't respect increments are always refused") =
    forAll(posNum[Int]) { bid =>
      val delta = bid - openned.currentPrice
      val notBigEnoughBid = delta < getMinIncrement(openned)
      notBigEnoughBid match {
        case true => {
          openned.receive(NewBid(Bid(bidder = joinedBidders.head, price = bid))) match {
            case Answer(StatusCodes.BadRequest, _) => true
            case _                                 => false
          }
        }
        case false => true
      }
    }

  property("Bids of bidders that did not join are always refused") =
    forAll(posNum[Int]) { bidder =>
      val bid = openned.currentPrice + getMinIncrement(openned)
      val bidderJoined = openned.bidders.contains(bidder)
      bidderJoined match {
        case false => {
          openned.receive(NewBid(Bid(bidder = bidder, price = bid))) match {
            case Answer(StatusCodes.BadRequest, _) => true
            case _                                 => false
          }
        }
        case true => true
      }
    }

  private def getMinIncrement(openned: Openned): Price = {
    if (openned.currentPrice <= firstSlot.endOfSlot) {
      firstSlot.minIncrement
    } else {
      lastSlot.minIncrement
    }
  }
}
