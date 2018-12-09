package com.ion.trials.akka.auction

import akka.actor.ActorRef
import AuctionTypes.{AuctionId, AuctionKey, AuctioneerId}

class Auctioneer(val auctioneerId: AuctioneerId) {
  val auctions = scala.collection.mutable.HashMap.empty[AuctionId, ActorRef]

  def get(auctionId: AuctionId): Option[ActorRef] = {
    auctions.get(auctionId)
  }

  def add(auctionId: AuctionId, auction: ActorRef): Unit = {
    auctions += auctionId -> auction
  }

  def getAllAuctions(): List[Tuple2[AuctionKey, ActorRef]] = {
    auctions.foldLeft(List.empty[Tuple2[AuctionKey, ActorRef]]) { case (res, (auctionId, actor)) =>
        Tuple2(AuctionKey(auctioneerId=auctioneerId, auctionId=auctionId), actor) :: res
    }
  }

}
