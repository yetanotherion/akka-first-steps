package com.spideo.hiring.ion.routes

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.spideo.hiring.ion.actors.AuctionHouseActor.CreateAuctionAnswer
import spray.json.DefaultJsonProtocol

trait JsonSupport extends SprayJsonSupport {
  import DefaultJsonProtocol._

  implicit val actionRuleParamsJsonFormat = jsonFormat5(AuctionRuleParams)
  implicit val auctionRulesParamsUpdateJsonFormat = jsonFormat5(AuctionRuleParamsUpdate)
}
