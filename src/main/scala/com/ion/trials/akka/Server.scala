package com.ion.trials.akka

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.RouteConcatenation
import akka.stream.ActorMaterializer
import com.ion.trials.akka.actors.AuctionHouseActor
import com.ion.trials.akka.service.{AuctionHouseService, SwaggerDocService}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Server extends App with RouteConcatenation {

  implicit val system = ActorSystem("auction-house")
  implicit val materializer = ActorMaterializer()

  implicit val executionContext = system.dispatcher

  val auctionHouseActor: ActorRef =
    system.actorOf(AuctionHouseActor.props, "auctionHouseActor")

  val routes = cors()(
    new AuctionHouseService(auctionHouseActor, system).routes ~ SwaggerDocService.routes)
  Http().bindAndHandle(routes, "localhost", 5000)

  println(s"Server online at http://localhost:5000/")

  Await.result(system.whenTerminated, Duration.Inf)

}
