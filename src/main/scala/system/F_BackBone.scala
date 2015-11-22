package system

import java.text.SimpleDateFormat

import akka.actor.{ActorRef, Props, Actor, ActorLogging}
import spray.http.{HttpRequest, HttpEntity}


class F_BackBone(f_pictureHandler: ActorRef, f_userHandler: ActorRef, f_pageProfileHandler: ActorRef) extends Actor with ActorLogging {
  import system.F_BackBone._

  def receive: Receive = {
    //GET functions
    case GetUserInfo(id) =>
      f_userHandler forward GetUserInfo(id)

    case GetPageInfo(id) =>
      f_pageProfileHandler forward GetPageInfo(id)

    case GetProfileInfo(id) =>
      f_pageProfileHandler forward GetPageInfo(id)

    case GetPictureInfo(id) =>
      f_pageProfileHandler forward GetPageInfo(id)

    case GetAlbumInfo(id) =>
      f_pictureHandler forward GetAlbumInfo(id)

    case GetImage(id) =>
      f_pictureHandler forward GetImage(id)

    //POST functions
    case UpdateUserData(id, req) =>
      f_userHandler forward UpdateUserData(id, req)

    case UpdatePageData(id, req) =>
      f_pageProfileHandler forward UpdatePageData(id, req)

    case UpdateProfileData(id, req) =>
      f_pageProfileHandler forward UpdateProfileData(id, req)

    case UpdateImageData(id, req) =>
      f_pictureHandler forward UpdateImageData(id, req)

    case UpdateAlbumData(id, req) =>
      f_pictureHandler forward UpdateAlbumData(id, req)

    case RequestFriend(id, req) =>
      f_userHandler forward RequestFriend(id, req)

    case AcceptFriend(id, req) =>
      f_userHandler forward RequestFriend(id, req)

    //PUT functions
    case PutImage(image) =>
      f_pictureHandler forward PutImage(image)

    case CreateUser(req) =>
      f_userHandler forward CreateUser(req)

    case CreatePage(req) =>
      f_pageProfileHandler forward CreatePage(req)

    case CreateAlbum(req) =>
      f_pictureHandler forward CreateAlbum(req)

    //DELETE functions
    case DeleteUser(id) =>
      f_userHandler forward DeleteUser(id)

    case DeletePage(id) =>
      f_pageProfileHandler forward DeletePage(id)

    case DeletePicture(id) =>
      f_pictureHandler forward DeletePicture(id)

    case DeleteAlbum(id) =>
      f_pictureHandler forward DeleteAlbum(id)

    //InterSystem messages
    case CreateUserProfile(userID) =>
      f_pageProfileHandler forward CreateUserProfile(userID)
  }
}

object F_BackBone {
  def props(f_pictureHandler: ActorRef, f_userHandler: ActorRef, f_pageProfileHandler: ActorRef) = Props(new F_BackBone(f_pictureHandler, f_userHandler, f_pageProfileHandler))

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

  case class CreateUserProfile(userID: BigInt) //replies with user profile id

  val dateFormatter = new SimpleDateFormat("'M'MM'D'dd'Y'yyyy")
}
