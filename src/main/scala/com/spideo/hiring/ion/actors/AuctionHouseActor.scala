package com.spideo.hiring.ion.actors

import akka.actor.{Actor, ActorLogging, Props}
import akka.http.scaladsl.model.StatusCodes
import com.spideo.hiring.ion.actors.Auction.PlannedMessage
import com.spideo.hiring.ion.auction.AuctionTypes._
import com.spideo.hiring.ion.auction.Auctioneer
import com.spideo.hiring.ion.routes.{AuctionRuleParams, AuctionRuleParamsUpdate}

import scala.util.{Failure, Success, Try}

object AuctionHouseActor {
  def props(): Props = Props(new AuctionHouseActor)

  private def toAuctionRule(params: AuctionRuleParams): Try[AuctionRule] = {
    try {
      Success(AuctionRule(startDate = toAuctionDate(params.startDate), endDate = toAuctionDate(params.endDate),
        item = params.item, initialPrice = params.initialPrice, increment = toIncrement(params.increment)))
    } catch {
      case e: IllegalArgumentException => Failure(e)
    }
  }

  final case object StartAuction

  final case class CreateAuction(auctioneerId: AuctioneerId, auctionId: AuctionId, auctionRule: AuctionRuleParams)
  final case class UpdateAuction(auctioneerId: AuctioneerId, auctionId: AuctionId, auctionRule: AuctionRuleParamsUpdate)

  //val errorInCreateAuction = CreateAuctionAnswer(None, msg="Got an error")
}

class AuctionHouseActor extends Actor with ActorLogging {

  import AuctionHouseActor._

  override def preStart(): Unit = log.info("AuctionHouse started")

  override def postStop(): Unit = log.info("AuctionHouse stopped")

  val auctioneers = scala.collection.mutable.HashMap.empty[AuctioneerId, Auctioneer]

  override def receive = {
    case CreateAuction(auctioneerId, auctionId, auctionRuleParams) => {
      toAuctionRule(auctionRuleParams) match {
        case Success(auctionRule) =>
          val ok = createAuction(auctioneerId, auctionId, auctionRule)
          if (ok) {
            sender() ! CreateAuctionAnswer(StatusCodes.Created, s"Auction $auctionId created by $auctioneerId")
          } else {
            sender() ! CreateAuctionAnswer(StatusCodes.Conflict,
              s"Auction $auctionId was already created by $auctioneerId")
          }
        case Failure(e) =>
          sender() ! CreateAuctionAnswer(StatusCodes.BadRequest, e.getMessage)
      }
    }
    case UpdateAuction(auctioneerId, auctionId, auctionRuleParamsUpdate) => {
      val auctioneer = getAuctioneer(auctioneerId)
      auctioneer.get(auctionId) match {
        case Some(actor) => {
          log.info(s"Forwarding $auctionRuleParamsUpdate")
          actor forward PlannedMessage(auctionRuleParamsUpdate)
        }
        case None => {
          sender () ! CreateAuctionAnswer(StatusCodes.NotFound, s"Auction $auctionId was not created by $auctioneerId")
        }
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
