package com.spideo.hiring.ion.routes

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import com.spideo.hiring.ion.actors.AuctionHouseActor.CreateAuctionAnswer
import spray.json.DefaultJsonProtocol

trait JsonSupport extends SprayJsonSupport {
  import DefaultJsonProtocol._

  implicit val actionRuleParamsJsonformat = jsonFormat5(AuctionRuleParams)
  implicit val createAuctionAnswerJsonFormat = jsonFormat2(CreateAuctionAnswer)
}
