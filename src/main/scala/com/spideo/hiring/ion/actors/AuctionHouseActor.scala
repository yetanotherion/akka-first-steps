package com.spideo.hiring.ion.actors

import java.text.ParseException

import akka.actor.{Actor, ActorLogging, Props}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.spideo.hiring.ion.auction.AuctionTypes._
import com.spideo.hiring.ion.auction.{AuctionRule, Auctioneer}
import com.spideo.hiring.ion.routes.AuctionRuleParams

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

  private val dateFormat = new java.text.SimpleDateFormat("yyyy/MM/dd-HH:mm")

  private def toAuctionDate(date: String): AuctionDate = {
    try {
      val res = dateFormat.parse(date)
      val epoch = res.getTime()
      epoch / 1000
    } catch {
      case e: ParseException => throw new IllegalArgumentException(s"$date is an invalid date")
    }
  }

  private def toIncrement(param: Integer): Increment = {
    Constant(param)
  }

  final case object StartAuction

  final case class CreateAuction(auctioneerId: AuctioneerId, auctionId: AuctionId, auctionRule: AuctionRuleParams)

  final case class CreateAuctionAnswer(status: StatusCode, msg: String)

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
