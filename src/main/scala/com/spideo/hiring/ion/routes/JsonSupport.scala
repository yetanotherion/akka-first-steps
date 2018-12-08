package com.spideo.hiring.ion.routes

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.spideo.hiring.ion.auction.AuctionTypes
import com.spideo.hiring.ion.auction.AuctionTypes.{AuctionDate, AuctionRule, Price}
import spray.json.{DefaultJsonProtocol, DeserializationException, JsArray, JsNumber, JsString, JsValue, RootJsonFormat}

trait JsonSupport extends SprayJsonSupport {
  import DefaultJsonProtocol._

  object AuctionDateProtocol extends DefaultJsonProtocol {
    implicit object AuctionRuleFormat extends RootJsonFormat[AuctionDate] {
      override def write(c: AuctionDate) = JsString(AuctionTypes.fromAuctionDate(c))

      override def read(value: JsValue) = value match {
        case JsString(date) => AuctionTypes.toAuctionDate(date)
        case _ => throw new DeserializationException("date expected ")
      }
    }
  }
  implicit val actionRuleParamsJsonFormat = jsonFormat5(AuctionRuleParams)
  implicit val auctionRulesParamsUpdateJsonFormat = jsonFormat5(AuctionRuleParamsUpdate)
  implicit val incrementJsonFormat = jsonFormat1(AuctionTypes.Increment)

  implicit val auctionDateJsonFormat = AuctionDateProtocol.AuctionRuleFormat
  implicit val auctionRuleJsonFormat = jsonFormat5(AuctionRule)

  final case class BidParam(bid: Price)
  implicit val bidParamJsonFormat = jsonFormat1(BidParam)
}
