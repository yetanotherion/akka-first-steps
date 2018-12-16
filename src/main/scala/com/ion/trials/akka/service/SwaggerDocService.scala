package com.ion.trials.akka.service

import com.github.swagger.akka.SwaggerHttpService
import com.github.swagger.akka.model.Info

object SwaggerDocService extends SwaggerHttpService {
  override val apiClasses =
    Set(classOf[AuctionHouseService], classOf[BidderService])
  override val host = "localhost:5000"
  override val info = Info(version = "1.0")
  override val unwantedDefinitions =
    Seq("Function1", "Function1RequestContextFutureRouteResult", "ListObject")
}
