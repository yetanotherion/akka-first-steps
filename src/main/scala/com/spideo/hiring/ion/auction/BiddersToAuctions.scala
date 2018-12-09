package com.spideo.hiring.ion.auction

import akka.actor.ActorRef
import com.spideo.hiring.ion.auction.AuctionTypes.{AuctionId, AuctionKey, AuctioneerId, Bidder}

class BiddersToAuctions {

  private val biddersToAuctions = scala.collection.mutable.HashMap.empty[Bidder, scala.collection.mutable.Set[Tuple2[AuctionKey, ActorRef]]]

  def addAuction(bidder: Bidder, auctioneerId: AuctioneerId, auctionId: AuctionId, actor: ActorRef): Unit = {
    val _ = biddersToAuctions
      .getOrElseUpdate(bidder, scala.collection.mutable.Set[Tuple2[AuctionKey, ActorRef]]())
      .add(Tuple2(AuctionKey(auctioneerId=auctioneerId, auctionId=auctionId), actor))
    ()
  }

  def getActors(bidder: Bidder): List[Tuple2[AuctionKey, ActorRef]] = {
    biddersToAuctions.getOrElse(bidder, List()).toList
  }

  def deleteAuctionFromBidder(bidder: Bidder, auctioneerId: AuctioneerId, auctionId: AuctionId): Unit = {
    biddersToAuctions.get(bidder).map {
      auctions =>
        val key = AuctionKey(auctionId = auctionId, auctioneerId = auctioneerId)
        biddersToAuctions += bidder -> auctions.filter(x => x._1 != key)
    }
  }

}
