package com.ion.trials.akka.util

import com.ion.trials.akka.actors.AuctionActor
import com.ion.trials.akka.auction.AuctionTypes
import com.ion.trials.akka.auction.AuctionTypes.{
  AuctionDate,
  AuctionInfo,
  AuctionKey,
  AuctionRule
}
import com.ion.trials.akka.auction.Planned.plannedStr
import com.ion.trials.akka.service.{AuctionRuleParams, AuctionRuleParamsUpdate}

import scala.concurrent.duration._
import scala.language.postfixOps

trait AuctionTestData {
  val auctioneerId = 1
  val auctionId = 1
  val currentTime = 10

  val startTime = currentTime + 1
  val endTime = startTime + 2

  val auctionRule = AuctionRule(startDate = AuctionDate(startTime),
                                endDate = AuctionDate(endTime),
                                item = 1,
                                initialPrice = 0,
                                increment = AuctionTypes.toIncrement(1))

  val auctionInfo = AuctionInfo(
    rule = auctionRule,
    state = AuctionActor.planned,
    bidders = List(),
    bids = List(),
    winner = None,
    currentPrice = None,
    auctionId = auctionId,
    auctioneerId = auctioneerId
  )

  val expectedMsgTimeout = 500 millis

  val emptyUpdate = AuctionRuleParamsUpdate(startDate = None,
                                            endDate = None,
                                            item = None,
                                            initialPrice = None,
                                            increment = None)

  val firstAuctionKey = AuctionKey(auctioneerId = 1, auctionId = 0)
  val sndAuctionKey = AuctionKey(auctioneerId = 2, auctionId = 0)

  val auctionRuleParams = AuctionRuleParams(
    startDate = AuctionTypes.fromAuctionDate(AuctionDate(startTime)),
    endDate = AuctionTypes.fromAuctionDate(AuctionDate(endTime)),
    item = 1,
    initialPrice = 0,
    increment = 1
  )

  def createExpectedAuctionInfo(key: AuctionKey): AuctionInfo = {
    AuctionInfo(rule = auctionRule,
                state = plannedStr,
                bidders = List(),
                bids = List(),
                winner = None,
                currentPrice = None,
                auctionId = key.auctionId,
                auctioneerId = key.auctioneerId)
  }

}
