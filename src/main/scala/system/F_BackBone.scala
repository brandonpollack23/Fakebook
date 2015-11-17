package system

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging}

class F_BackBone extends Actor with ActorLogging {
  override def receive: Receive = ???
}

object F_BackBone {
  def props = ???

  sealed trait GetInfo
  case class GetUserInfo(id: BigInt) extends GetInfo
  case class GetPageInfo(id: BigInt) extends GetInfo
  case class GetProfileInfo(id: BigInt) extends GetInfo
  case class GetPictureInfo(id: BigInt) extends GetInfo
  case class GetAlbumInfo(id: BigInt) extends GetInfo
  case class GetImage(id: BigInt) extends GetInfo
}
