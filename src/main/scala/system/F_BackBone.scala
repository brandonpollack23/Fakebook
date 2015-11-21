package system

import akka.actor.{Props, Actor, ActorLogging}
import spray.http.{HttpHeader, HttpRequest, HttpEntity}

class F_BackBone extends Actor with ActorLogging {
  override def receive: Receive = ???
}

object F_BackBone {
  def props = Props[F_BackBone]

  sealed trait GetInfo //for id doesnt exist make sure to throw a failure with that in the message, this will make the future respond like this
  //To complete the future with an exception you need send a Failure message to the sender. This is not done automatically when an actor throws an exception while processing a message. akka.actor.Status.Failure(exception)
  case class GetUserInfo(id: BigInt) extends GetInfo
  case class GetPageInfo(id: BigInt) extends GetInfo
  case class GetProfileInfo(id: BigInt) extends GetInfo
  case class GetPictureInfo(id: BigInt) extends GetInfo
  case class GetAlbumInfo(id: BigInt) extends GetInfo
  case class GetImage(id: BigInt) extends GetInfo

  sealed trait PostInfo
  case class UpdateUserData(id: BigInt, httpRequest: HttpRequest) extends PostInfo
  case class UpdatePageData(id: BigInt, httpRequest: HttpRequest) extends PostInfo
  case class UpdateProfileData(id: BigInt, httpRequest: HttpRequest) extends PostInfo
  case class UpdateImageData(id: BigInt, httpRequest: HttpRequest) extends PostInfo
  case class UpdateAlbumData(id: BigInt, httpRequest: HttpRequest) extends PostInfo
  case class RequestFriend(id: BigInt, httpRequest: HttpRequest) extends PostInfo
  case class AcceptFriend(id: BigInt, httpRequest: HttpRequest) extends PostInfo //restful path is the user accepting, parameter is accepting friend

  sealed trait PutInfo //note: you can use the routing DSL parameter seq to extract parameters!
  case class PutImage(image: HttpEntity) extends PutInfo//must send the original sender back the JSON object of the created image
  case class CreateUser(httpRequest: HttpRequest) extends PutInfo //create user user arguments stored in httprequest and return new user JSON, they need a default profile, album, and unfilled fields for name etc
  case class CreatePage(httpRequest: HttpRequest) extends PutInfo //create page and return JSON
  case class CreateAlbum(httpRequest: HttpRequest) extends PutInfo //create picture and return JSON

  sealed trait DeleteInfo
  case class DeleteUser(id: BigInt) extends DeleteInfo
  case class DeletePage(id: BigInt) extends DeleteInfo
  case class DeletePicture(id: BigInt) extends DeleteInfo
  case class DeleteAlbum(id: BigInt) extends DeleteInfo //will not delete default album, deletes all pictures in album
}
