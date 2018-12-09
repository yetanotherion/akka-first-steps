package com.ion.trials.akka

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.ion.trials.akka.actors.AuctionHouseActor
import com.ion.trials.akka.routes.AuctionHouseRoutes

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Server extends App with AuctionHouseRoutes {

  implicit val system = ActorSystem("auction-house")
  implicit val materializer = ActorMaterializer()

  implicit val executionContext = system.dispatcher

  val auctionHouseActor: ActorRef =
    system.actorOf(AuctionHouseActor.props, "auctionHouseActor")

  lazy val routes: Route = auctionHouseRoutes

  Http().bindAndHandle(routes, "localhost", 5000)

  println(s"Server online at http://localhost:5000/")

  Await.result(system.whenTerminated, Duration.Inf)

}
