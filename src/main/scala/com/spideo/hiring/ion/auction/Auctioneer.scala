package com.spideo.hiring.ion.auction

import akka.actor.ActorRef
import com.spideo.hiring.ion.auction.AuctionTypes.AuctionId

class Auctioneer {
  val auctions = scala.collection.mutable.HashMap.empty[AuctionId, ActorRef]

  def get(auctionId: AuctionId): Option[ActorRef] = {
    auctions.get(auctionId)
  }

  def add(auctionId: AuctionId, auction: ActorRef): Unit = {
    auctions += auctionId -> auction
  }

}
