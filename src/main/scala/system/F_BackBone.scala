package system

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging}
import spray.http.{HttpRequest, HttpEntity}

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

  sealed trait PutInfo
  case class PutImage(image: HttpEntity) extends PutInfo//must send the original sender back the JSON object of the created image
  case class CreateUser(httpRequest: HttpRequest) extends PutInfo //create user user arguments stored in httprequest and return new user JSON, they need a default profile, album, and unfilled fields for name etc
  case class CreatePage(httpRequest: HttpRequest) extends PutInfo //create page and return JSON
  case class CreateAlbum(httpRequest: HttpRequest) extends PutInfo //create picture and return JSON
}
