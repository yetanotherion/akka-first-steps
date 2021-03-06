package com.ion.trials.akka.service

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.ion.trials.akka.actors.GatherBidsOfBidderActor.BidsOfBidder
import com.ion.trials.akka.actors.GatherAuctionsActor.AuctionInfos
import com.ion.trials.akka.auction.AuctionTypes
import com.ion.trials.akka.auction.AuctionTypes._
import com.ion.trials.akka.service.AuctionService.{
  AuctionRuleParams,
  AuctionRuleParamsUpdate
}
import spray.json.{
  DefaultJsonProtocol,
  DeserializationException,
  JsString,
  JsValue,
  RootJsonFormat
}

trait JsonSupport extends SprayJsonSupport {
  import DefaultJsonProtocol._

  object AuctionDateProtocol extends DefaultJsonProtocol {
    implicit object AuctionRuleFormat extends RootJsonFormat[AuctionDate] {
      override def write(c: AuctionDate) =
        JsString(AuctionTypes.fromAuctionDate(c))

      override def read(value: JsValue) = value match {
        case JsString(date) => AuctionTypes.toAuctionDate(date)
        case _              => throw new DeserializationException("date expected ")
      }
    }
  }

  implicit val errorJsonFormat = jsonFormat1(Error)

  implicit val incrementSlotJsonFormat = jsonFormat2(AuctionTypes.IncrementSlot)
  implicit val incrementJsonFormat = jsonFormat1(AuctionTypes.Increment)
  implicit val incrementParamsJsonFormat = jsonFormat2(
    AuctionService.IncrementParams)

  implicit val actionRuleParamsJsonFormat = jsonFormat5(AuctionRuleParams)
  implicit val auctionRulesParamsUpdateJsonFormat = jsonFormat5(
    AuctionRuleParamsUpdate)

  implicit val auctionDateJsonFormat = AuctionDateProtocol.AuctionRuleFormat
  implicit val auctionRuleJsonFormat = jsonFormat5(AuctionRule)

  implicit val bidParamJsonFormat = jsonFormat1(BidParam)
  implicit val bidJsonFormat = jsonFormat2(Bid)

  implicit val auctionJsonFormat = jsonFormat8(AuctionInfo)

  implicit val bidsOfBidderJsonFormat = jsonFormat7(BidsOfBidderInOneAuction)
  implicit val bidsOfBiddersAnswersJsonFormat = jsonFormat1(BidsOfBidder)
  implicit val auctionInfosJsonFormat = jsonFormat1(AuctionInfos)

}
