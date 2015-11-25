package clientSim


import spray.http._
import HttpMethods._
import akka.pattern.ask
import akka.actor.Actor
import spray.can.Http
import akka.io.IO
import spray.json._
import akka.actor._
import DefaultJsonProtocol._
import java.util._
import scala.concurrent.Future
import spray.httpx.SprayJsonSupport
import spray.json.AdditionalFormats
import graphnodes._



class F_BaseClient extends Actor with SprayJsonSupport with AdditionalFormats{

  case class createPost(id : BigInt)
  case class createPage(id : BigInt)
  case class createAlbum(id : BigInt)
  case class uploadPicture(id : BigInt)
  case class updateUserData(id: BigInt)
  case class updatePictureData(id : BigInt)
  case class updateAlbumData(id :BigInt)
  case class updatePost(id : BigInt)
  case class updatePageData(id : BigInt)
  case class getUserData(id :BigInt)
  case class getImageData(id : BigInt)
  case class getAlbumData(id : BigInt)
  case class getPost(id : BigInt)
  case class getAlbumData(id : BigInt)
  case class getPageData(id :BigInt)
  case class deleteUser(id :BigInt)
  case class deleteImage(id :BigInt)
  case class deleteAlbum(Id :BigInt)
  case class deletePost( id : BigInt)
  case class deletePage(id :BigInt)
  case class sendFriendReq(id1 : BigInt, id2 :BigInt)
  case class acceptFriendReq(id1 :BigInt, id2 :BigInt)

def receive = {

  //Create operations
  case createUser =>
    val uri = Uri("https://www.fakebook.com/newuser/") withQuery(F_User.lastNameString -> "",F_User.firstNameString -> "", F_User.bioString -> "", F_User.dobString -> "", F_User.ageString -> "")
    sender !  (IO(Http) ? HttpRequest(PUT, uri)).mapTo[HttpResponse]

  case createPost(id) =>
    val uri = Uri("https://www.fakebook.com/newpost/") withQuery(F_Post.creator -> id.toString(16),F_Post.contents -> "", F_Post.locationType -> "", F_Post.dateOfCreation -> "")
    sender !  (IO(Http) ? HttpRequest(PUT, uri)).mapTo[HttpResponse]

  case createPage(id) =>
    val uri = Uri("https://www.fakebook.com/newpage/") withQuery( F_Page.ownerID -> id.toString(16), F_Page.name -> "", F_Page.decsription -> "", F_Page.dateOfCreation -> "")
    sender !  (IO(Http) ? HttpRequest(PUT, uri)).mapTo[HttpResponse])

  case createAlbum(id) =>
    val uri = Uri("https://www.fakebook.com/createalbum/") withQuery(F_Album.ownerString -> id.toString(16), F_Album.nameString -> "", F_Album.descriptionString -> "")
    sender !  (IO(Http) ? HttpRequest(PUT, uri)).mapTo[HttpResponse])

  case uploadPicture(id) =>
    val uri = Uri("https://www.fakebook.com/data/uploadimage/") withQuery(F_Picture.ownerString -> id.toString(16), F_Picture.nameString -> "", F_Picture.descriptionString -> "")
    sender !  (IO(Http) ? HttpRequest(PUT, uri)).mapTo[HttpResponse])

  //Update operations
  case updateUserData(id) =>
    val uri = Uri("https://www.fakebook.com/user/request/") withQuery(F_User.ID-> id.toString(16) ,F_User.lastNameString -> "",F_User.firstNameString -> "", F_User.bioString -> "", F_User.dobString -> "", F_User.ageString -> "")
    sender !  (IO(Http) ? HttpRequest(POST, uri)).mapTo[HttpResponse])

  case updatePictureData(id) =>
    val uri = Uri("https://www.fakebook.com/picture/") withQuery(F_Picture.ownerString -> id.toString(16), F_Picture.nameString -> "", F_Picture.descriptionString -> "")
    sender !  (IO(Http) ? HttpRequest(POST, uri)).mapTo[HttpResponse])

  case updateAlbumData(id) =>
    val uri = Uri("https://www.fakebook.com/album/") withQuery(F_Album.ownerString -> id.toString(16), F_Album.nameString -> "", F_Album.descriptionString -> "")
    sender !  (IO(Http) ? HttpRequest(POST, uri)).mapTo[HttpResponse])

  case updatePost(id) =>
    val uri = Uri("https://www.fakebook.com/post/") withQuery( F_Post.creator -> id.toString(16), F_Post.contents -> "", F_Post.locationType -> "", F_Post.dateOfCreation -> "")
    sender !  (IO(Http) ? HttpRequest(POST, uri)).mapTo[HttpResponse])

  case updatePageData(id) => //TODO distinction between multiple pages of same user by page name only??
    val uri = Uri("https://www.fakebook.com/page/") withQuery(F_Page.name -> "", F_Page.decsription -> "", F_Page.dateOfCreation -> "")
    sender !  (IO(Http) ? HttpRequest(POST, uri)).mapTo[HttpResponse])

  //get operations
  case getUserData(id) =>
    val uri = Uri("https://www.fakebook.com/user/") withQuery(F_User.lastNameString -> "",F_User.firstNameString -> "", F_User.bioString -> "", F_User.dobString -> "", F_User.ageString -> "")
    sender !  (IO(Http) ? HttpRequest(GET, uri)).mapTo[HttpResponse])

  case getImageData(id) =>
    val uri = Uri("https://www.fakebook.com/picture/") withQuery( F_Picture.ownerString -> id.toString(16), F_Picture.nameString -> "", F_Picture.descriptionString -> "")
    sender !  (IO(Http) ? HttpRequest(GET, uri)).mapTo[HttpResponse])

  case getAlbumData(id) =>
    val uri = Uri("https://www.fakebook.com/album/") withQuery(F_Album.ownerString -> id.toString(16), F_Album.nameString -> "", F_Album.descriptionString -> "")
    sender !  (IO(Http) ? HttpRequest(GET, uri)).mapTo[HttpResponse])

  case getPost(id) =>
    val uri = Uri("https://www.fakebook.com/post/") withQuery(F_Post.creator -> id.toString(16), F_Post.contents -> "", F_Post.locationType -> "", F_Post.dateOfCreation -> "")
    sender !  (IO(Http) ? HttpRequest(GET, uri)).mapTo[HttpResponse])

  case getPageData(id) =>
    val uri = Uri("https://www.fakebook.com/page/") withQuery(F_Page.ownerID -> id.toString(16) ,F_Page.name -> "", F_Page.description -> "",  F_Post.dateOfCreation -> "")
    sender !  (IO(Http) ? HttpRequest(GET, uri)).mapTo[HttpResponse])

  //Delete operations
  case deleteUser(id) =>
    val uri = Uri("https://www.fakebook.com/user/") withQuery(F_User.ID -> id.toString(16), F_User.lastNameString -> "",F_User.firstNameString -> "", F_User.bioString -> "", F_User.dobString -> "", F_User.ageString -> "")
    sender !  (IO(Http) ? HttpRequest(DELETE, uri)).mapTo[HttpResponse])

  case deleteImage(id) =>
    val uri = Uri("https://www.fakebook.com/picture/") withQuery(F_Picture.nameString -> "", F_Picture.descriptionString -> "", F_Picture.ownerString -> "")
    sender !  (IO(Http) ? HttpRequest(DELETE, uri)).mapTo[HttpResponse])

  case deleteAlbum(id) =>
    val uri = Uri("https://www.fakebook.com/album/") withQuery(F_Album.nameString -> "", F_Album.descriptionString -> "", F_Album.ownerString -> "")
    sender !  (IO(Http) ? HttpRequest(DELETE, uri)).mapTo[HttpResponse])

  case deletePost(id) =>
    val uri = Uri("https://www.fakebook.com/post/") withQuery(F_Post.contents -> "", F_Post.creator -> id, F_Post.locationType -> "", F_Post.dateOfCreation -> "")
    sender !  (IO(Http) ? HttpRequest(DELETE, uri)).mapTo[HttpResponse])

  case deletePage(id) =>
    val uri = Uri("https://www.fakebook.com/page/") withQuery(F_Post.contents -> "", F_Post.creator -> id, F_Post.locationType -> "", F_Post.dateOfCreation -> "")
    sender !  (IO(Http) ? HttpRequest(GET, uri)).mapTo[HttpResponse])

  //Friend operations
  case sendFriendReq(id1, id2) =>
    val uri = Uri("https://www.fakebook.com/user/resuest/") withQuery(F_User.ID -> id1.toString(16))  //send user id self and requested users
    sender !  (IO(Http) ? HttpRequest(PUT, uri)).mapTo[HttpResponse])

  case acceptFriendReq(id1, id2) =>
    //do we need this before authentication part??




}


}
