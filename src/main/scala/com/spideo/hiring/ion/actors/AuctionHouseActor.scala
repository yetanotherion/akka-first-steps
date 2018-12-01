package com.spideo.hiring.ion.actors

import akka.actor.{Actor, ActorLogging, Props}
import com.spideo.hiring.ion.auction.AuctionTypes._

import com.spideo.hiring.ion.auction.{AuctionRule, Auctioneer}
import com.spideo.hiring.ion.routes.AuctionRuleParams

object AuctionHouseActor {
  def props(): Props = Props(new AuctionHouseActor)

  private def toAuctionRule(params: AuctionRuleParams): AuctionRule = {
    AuctionRule(startDate=toAuctionDate(params.startDate), endDate=toAuctionDate(params.endDate),
      item=params.item,initialPrice=params.initialPrice, increment=toIncrement(params.increment))
  }

  private val dateFormat = new java.text.SimpleDateFormat("yyyy/MM/dd-HH:mm")

  private def toAuctionDate(date: String): AuctionDate = {
    val res = dateFormat.parse(date)
    val epoch = res.getTime()
    epoch / 1000
  }

  private def toIncrement(param: String): Increment = {
    Constant(0)
  }
  final case object StartAuction
  final case class CreateAuction(auctioneerId: AuctioneerId, auctionId: AuctionId, auctionRule: AuctionRuleParams)
  final case class CreateAuctionAnswer(alreadyCreated: Boolean, msg: String)
  //val errorInCreateAuction = CreateAuctionAnswer(None, msg="Got an error")
}

class AuctionHouseActor extends Actor with ActorLogging {

  import AuctionHouseActor._

  override def preStart(): Unit = log.info("AuctionHouse started")

  override def postStop(): Unit = log.info("AuctionHouse stopped")

  val auctioneers = scala.collection.mutable.HashMap.empty[AuctioneerId, Auctioneer]

  override def receive = {
    case CreateAuction(auctioneerId, auctionId, auctionRule) => {
      val ok = createAuction(auctioneerId, auctionId, toAuctionRule(auctionRule))
      if (ok) {
        sender () ! CreateAuctionAnswer(false, s"Auction $auctionId created by $auctioneerId")
      } else {
        sender () ! CreateAuctionAnswer(true, s"Auction $auctionId was already created by $auctioneerId")
      }
    }
  }

  def getAuctioneer(auctioneerId: AuctioneerId): Auctioneer = {
    auctioneers.getOrElseUpdate(auctioneerId, new Auctioneer())
  }

  def createAuction(auctioneerId: AuctioneerId, auctionId: AuctionId, auctionRule: AuctionRule): Boolean = {
    val auctioneer = getAuctioneer(auctioneerId)
    auctioneer.get(auctionId) match {
      case Some(_) => false
      case None =>
        val auctionProps = Auction.props(auctionRule)
        val res = context.actorOf(auctionProps)
        getAuctioneer(auctioneerId).add(auctionId, res)
        true
    }
  }
}
