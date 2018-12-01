package com.spideo.hiring.ion

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.spideo.hiring.ion.actors.AuctionHouseActor
import com.spideo.hiring.ion.routes.AuctionHouseRoutes

import scala.concurrent.Await
import scala.concurrent.duration.Duration


object Server extends App with AuctionHouseRoutes {

    implicit val system = ActorSystem("auction-house")
    implicit val materializer = ActorMaterializer()

    implicit val executionContext = system.dispatcher

    val auctionHouseActor: ActorRef = system.actorOf(AuctionHouseActor.props, "auctionHouseActor")

    //#main-class
    lazy val routes: Route = auctionHouseRoutes

    //#http-server
    Http().bindAndHandle(routes, "localhost", 5000)

    println(s"Server online at http://localhost:5000/")

    Await.result(system.whenTerminated, Duration.Inf)
}