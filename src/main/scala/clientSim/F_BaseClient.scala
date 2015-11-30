package clientSim

import akka.io.IO
import spray.json
import system.{MyJsonProtocol, F_Server}

//import spray.json._
import spray.http._
import HttpMethods._
import spray.can.Http
//import akka.pattern.ask
//import akka.util.Timeout
//import scala.concurrent._
//import scala.concurrent.Future
//import scala.concurrent.duration._
//import spray.json.{JsonFormat, DefaultJsonProtocol}
//import scala.concurrent.ExecutionContext.Implicits.global._

import graphnodes._
import akka.actor._
import akka.util.Timeout
import akka.actor.Actor
import akka.actor.ActorSystem
import spray.http._
import spray.client.pipelining._
import spray.httpx.SprayJsonSupport
import spray.client.pipelining.sendReceive
import scala.util.{Success, Failure}
import MyJsonProtocol._
import scala.concurrent.duration._
import java.text.SimpleDateFormat
import java.util.Date
import MatchClasses._

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.util.Timeout
import akka.pattern.ask
import akka.io.IO

import spray.can.Http
import spray.http._
import HttpMethods._

import MyJsonProtocol._
import spray.json._

class F_BaseClient extends Actor with ActorLogging with SprayJsonSupport with AdditionalFormats
{


  log.debug("BaseClient logging started")
/*
  case class createUser(fname:String, lName:String, bio:String, age:Int, dob:Date)
  case class createPost(posterId: BigInt, content:String,locationType:String, locationId : BigInt)
  case class createPage(userId : BigInt, pName:String, pDes:String)
  case class createAlbum(id : BigInt, albmName:String, albmDes:String)
  case class uploadPicture(pName:String, pDes:String, albumID : BigInt, userId: BigInt)

  case class updateUserData(userID: BigInt, fName:String, lName:String, bio:String)
  case class updateUserProfile(profId: BigInt, des:String)
  case class updatePictureData(pName:String, pDes:String, albumID : BigInt, userId: BigInt)
  case class updateAlbumData(useId :BigInt, albmId:BigInt, albmName:String, albmDes:String)
  case class updatePost(id : BigInt, postID: BigInt)
  case class updatePageData(userId : BigInt, pName:String, pDes:String)

  case class getUserData(id :BigInt)
  case class getUserProfile(id : BigInt)
  case class getPictureData(userId : BigInt, picID:BigInt)
  case class getAlbumData(userId : BigInt, albmId:BigInt)
  case class getPost(userId : BigInt, postId:BigInt)
  case class getPageData(id :BigInt)

  case class deleteUser(id :BigInt)
  case class deletePicture(userId :BigInt, picId:BigInt)
  case class deleteAlbum(userId :BigInt, albmId:BigInt)
  case class deletePost( id : BigInt, postId:BigInt)
  case class deletePage(id :BigInt)

  case class sendFriendReq(userId : BigInt, frndId :BigInt)
  case class acceptFriendReq(userId :BigInt, frndId :BigInt)

  case class userCreated(res : F_User)
  case class postCreated(res : F_Post, locationType:String)
  case class pageCreated(res : F_Page)
  case class pictureUploaded(res : F_Picture)
  case class albumCreated(res : F_Album)
  case class profileCreated(res : F_UserProfile)

  case class userEdited(res : F_User)
  case class userProfileEdited(res : F_UserProfile)
  case class postEdited(res : F_Post)
  case class pageEdited(res : F_Page)
  case class pictureEdited(res : F_Picture)
  case class albumEdited(res : F_Album)
  case class profileEdited(res : F_UserProfile)

  case class userRetrieved(res : F_User)
  case class userProfileRetrieved(res : F_UserProfile)
  case class postRetrieved(res : F_Post)
  case class pageRetrieved(res : F_Page)
  case class pictureRetrieved(res : F_Picture)
  case class albumRetrieved(res : F_Album)
  case class profileRetrieved(res : F_UserProfile)

  case class userDeleted(res : F_User)
  case class postDeleted(res : F_Post)
  case class pageDeleted(res : F_Page)
  case class pictureDeleted(res : F_Picture)
  case class albumDeleted(res : F_Album)
  case class profileDeleted(res : F_UserProfile)

  case class friendRequestSent()
  case class friendRequestReceived()
  case class friendRequestAccepted()
  case class friendDeleted()
*/
  implicit val system = ActorSystem()
  import system.dispatcher
  implicit val requestTimeout = Timeout(5 seconds)
  val dateFormatter = new SimpleDateFormat("'M'MM'D'dd'Y'yyyy")

def receive = {


  //Create operations
  case createUser(fname, lname, bio, age, dob) =>

    val uri = Uri("http://localhost:8080/users/newuser") withQuery(F_User.lastNameString -> lname,
                                                                      F_User.firstNameString -> fname,
                                                                      F_User.bioString -> bio,
                                                                      F_User.ageString -> age.toString,
                                                                      F_User.dobString -> dateFormatter.format(dob))

    //val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
    // val response: Future[HttpResponse] = pipeline(Get(uri))
    log.info("=>> createUser sending request...")
    //val handler:ActorRef = system.actorOf(F_Server.props,"handler")
    //IO(Http) ! Http.Bind(handler, "localhost", port =8080)
    // IO(Http) ! Http.Connect("localhost", port = 8080)
    //IO(Http) ! Http.HostConnectorSetup("localhost", port = 8080)
    //val response: Future[HttpResponse] = (IO(Http) ? HttpRequest(PUT, uri)).mapTo[HttpResponse]
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(jsonUser) =>
        log.info("createUser successful!!")
        sender ! userCreated(jsonUser.parseJson.convertTo[F_User])

      case Failure(error) =>
        log.error(error, "Couldn't run createUser because of " + error.getMessage)

    }

  case createPost(posterId, content, locationType, locationId) =>
    val uri = Uri("http://localhost:8080/newpost") withQuery( F_Post.contentsString -> content,
                                                                  F_Post.creatorString -> posterId.toString(16),
                                                                  F_Post.locationType -> locationType,
                                                                  F_Post.locationTypeString -> "",
                                                                  F_Post.locationPage -> "",
                                                                  F_Post.locationProfile -> "",
                                                                  //F_Post.changableParamaters -> List(" "),
                                                                  F_Post.locationString -> locationId.toString())
    log.info("=>> createPost sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Post]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(f: F_Post) =>
        log.info("createPost successful!!")
        sender ! postCreated(f, locationType)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during createPost?")

      case Failure(error) =>
        log.error(error, "Couldn't run createPost :(")

    }

  case createPage(userId, pName, pDes) =>
    val uri = Uri("http://localhost:8080/page/newpage") withQuery( F_Page.joinPageString -> "",
                                                                  F_Page.leavePageString -> "",
                                                                  F_Page.newUserString -> "",
                                                                  F_Page.nameString -> pName,
                                                                  F_Page.descriptionString -> pDes,
                                                                  //F_Page.changableParameters -> ,
                                                                  F_Page.ownerString -> userId.toString(16))
    log.info("=>> createPage sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Page]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(f: F_Page) =>
        log.info("Page creation successful!!")
        sender ! pageCreated(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during Page creation?")

      case Failure(error) =>
        log.error(error, "Couldn't get Page Created :(")

    }

  case createAlbum(userId, albmName, albmDes) =>
    val uri = Uri("http://localhost:8080/album/createalbum") withQuery( F_Album.ownerString -> userId.toString(16),
                                                                      F_Album.nameString -> albmName,
                                                                      //F_Album.changableParameters -> ,
                                                                      F_Album.descriptionString -> albmDes)
    log.info("=>> createAlbum sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Album]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(f: F_Album) =>
        log.info("createAlbum successful!!")
        sender ! albumCreated(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during createAlbum?")

      case Failure(error) =>
        log.error(error, "Couldn't run createAlbum :(")

    }

  case uploadPicture(pName, pDes, albumId, userId) =>
    val uri = Uri("http://localhost:8080/data/uploadimage") withQuery(F_Picture.ownerString -> userId.toString(16),
                                                                          F_Picture.nameString -> pName,
                                                                          F_Picture.descriptionString -> pDes,
                                                                          //F_Picture.changableParameters -> ,
                                                                          F_Picture.albumString -> albumId.toString(16))

    log.info("=>> uploadPicture, sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Picture]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(f: F_Picture) =>
        log.info("uploadPicture successful!!")
        sender ! pictureUploaded(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during uploadPicture?")

      case Failure(error) =>
        log.error(error, "Couldn't run uploadPicture :(")

    }



  //Update operations
  case updateUserData(id, fName, lName, bio) =>        //TODO user object doesn't have id string
    val uri = Uri("http://localhost:8080/users/request") withQuery(F_User.lastNameString -> lName,
                                                                      F_User.firstNameString -> fName,
                                                                      F_User.bioString -> bio,
                                                                      F_User.dobString -> "",
                                                                      F_User.ageString -> "",
                                                                      // F_User.changableParameters -> ,
                                                                      F_User.friendRequestString -> "",
                                                                      F_User.acceptFriendString -> "",
                                                                      F_User.friendRemoveString -> "")
    log.info("=>> updateUserData, sending request...")
    val pipeline = sendReceive ~> unmarshal[F_User]
    val responseFuture = pipeline {Post(uri)}
    responseFuture onComplete {
      case Success(f: F_User) =>
        log.info("updateUserData successful!!")
        sender ! userEdited(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during updateUserData?")

      case Failure(error) =>
        log.error(error, "Couldn't run updateUserData :(")

    }


  case updateUserProfile(profId, des) =>      //TODO no identifier for profile either
    val uri = Uri("http://localhost:8080/profile") withQuery( F_UserProfile.profilePictureString -> "",
                                                                  // F_User.changableParameters -> ,
                                                                  F_UserProfile.descriptionString -> des)
    log.info("=>> updateUserProfile sending request...")
    val pipeline = sendReceive ~> unmarshal[F_UserProfile]
    val responseFuture = pipeline {Post(uri)}

    responseFuture onComplete {
      case Success(f: F_UserProfile) =>
        log.info("updateUserData successful!!")
        sender ! userProfileEdited(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during upadateUserProfile?")

      case Failure(error) =>
        log.error(error, "Couldn't run updateUserProfile :(")

    }


  case updatePictureData(pName, pDes, userId, pId) =>     //TODO no way to identify picture without unique picture id
    val uri = Uri("http://localhost:8080/picture") withQuery( F_Picture.ownerString -> userId.toString(16),
                                                                  F_Picture.nameString -> pName,
                                                                  F_Picture.descriptionString -> pDes,
                                                                  //F_Picture.changableParameters -> ,
                                                                  F_Picture.albumString -> "")

    log.info("=>> createPictureData, sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Picture]
    val responseFuture = pipeline {Post(uri)}
    responseFuture onComplete {
      case Success(f: F_Picture) =>
        log.info("updatePictureData successful!!")
        sender ! pictureEdited(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during updatePictureData?")

      case Failure(error) =>
        log.error(error, "Couldn't run updatePictureData :(")

    }

  case updateAlbumData(userId, albmId, albmName, albmDes) =>
    val uri = Uri("http://localhost:8080/album") withQuery( F_Album.ownerString -> userId.toString(16),
                                                                F_Album.nameString -> albmName,
                                                                //F_Album.changableParameters ->
                                                                F_Album.descriptionString -> albmDes)
    log.info("=>> updateAlbumData, sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Album]
    val responseFuture = pipeline {Post(uri)}

    responseFuture onComplete {
      case Success(f: F_Album) =>
        log.info("updateAlbumData successful!!")
        sender ! albumEdited(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during updateAlbumData?")

      case Failure(error) =>
        log.error(error, "Couldn't run updateAlbumData :(")

    }

  case updatePost(id, postId) =>
    val uri = Uri("http://localhost:8080/post") withQuery(F_Post.contentsString -> "",
                                                              F_Post.creatorString -> id.toString(16),
                                                              F_Post.locationType -> "",
                                                              F_Post.locationTypeString -> "",
                                                              F_Post.locationPage -> "",
                                                              F_Post.locationProfile -> "",
                                                              //F_Post.changableParamaters ->
                                                              F_Post.locationString -> postId.toString(16))
    log.info("=>> updatePost sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Post]
    val responseFuture = pipeline {Post(uri)}

    responseFuture onComplete {
      case Success(f: F_Post) =>
        log.info("updatePost successful!!")
        sender ! postEdited(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during upatePost?")

      case Failure(error) =>
        log.error(error, "Couldn't run updatePost :(")

    }

  case updatePageData(userId, pName, pDes) =>         //TODO no unique page ID to identify it
    val uri = Uri("http://localhost:8080/page") withQuery(F_Page.joinPageString -> "",
                                                              F_Page.leavePageString -> "",
                                                              F_Page.newUserString -> "",
                                                              F_Page.nameString -> pName,
                                                              F_Page.descriptionString -> pDes,
                                                              //F_Page.changableParameters -> ,
                                                              F_Page.ownerString -> userId.toString(16))
    log.info("=>> updatePageData sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Page]
    val responseFuture = pipeline {Post(uri)}

    responseFuture onComplete {
      case Success(f: F_Page) =>
        log.info("updatePageData successful!!")
        sender ! pageEdited(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during updatePageData?")

      case Failure(error) =>
        log.error(error, "Couldn't run updatePageData :(")

    }



  //get operations
  case getUserData(id) =>             //TODO no unique user id in user object in F_User
    val uri = Uri("http://localhost:8080/users") withQuery(F_User.lastNameString -> "",
                                                              F_User.firstNameString -> "",
                                                              F_User.bioString -> "",
                                                              F_User.dobString -> "",
                                                              F_User.ageString -> "",
                                                              // F_User.changableParameters -> ,
                                                              F_User.friendRequestString -> "",
                                                              F_User.acceptFriendString -> "",
                                                              F_User.friendRemoveString -> "")
    log.info("=>> getUserData sending request...")
    val pipeline = sendReceive ~> unmarshal[F_User]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(f: F_User) =>
        log.info("getUserData successful!!")
        sender ! userRetrieved(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during getUserData?")

      case Failure(error) =>
        log.error(error, "Couldn't run getUserData :(")

    }


  case getUserProfile(id) =>
    val uri = Uri("http://localhost:8080/profile") withQuery( F_UserProfile.profilePictureString -> "",
                                                                  // F_User.changableParameters -> ,
                                                                  F_UserProfile.descriptionString -> "")
    log.info("=>> getUserProfile sending request...")
    val pipeline = sendReceive ~> unmarshal[F_UserProfile]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(f: F_UserProfile) =>
        log.info("getUserData successful!!")
        sender ! userProfileRetrieved(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during getUserProfile?")

      case Failure(error) =>
        log.error(error, "Couldn't run getUserProfile :(")

    }


  case getPictureData(id, picId) =>              //TODO picture object doesn't have uniqueID for pic identification
    val uri = Uri("http://localhost:8080/picture") withQuery( F_Picture.ownerString -> id.toString(16),
                                                                  F_Picture.nameString -> "",
                                                                  F_Picture.descriptionString -> "",
                                                                  //F_Picture.changableParameters -> ,
                                                                  F_Picture.albumString -> "")
    log.info("=>> getPictureData sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Picture]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(f: F_Picture) =>
        log.info("getPictureData successful!!")
        sender ! pictureRetrieved(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during getPictureData?")

      case Failure(error) =>
        log.error(error, "Couldn't run getPictureData :(")

    }

  case getAlbumData(userId, albmId) =>
    val uri = Uri("http://localhost:8080/album") withQuery( F_Album.ownerString -> userId.toString(16),
                                                                F_Album.nameString -> "",
                                                                //F_Album.changableParameters ->
                                                                F_Album.descriptionString -> "")
    log.info("=>> getAlbumData, sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Album]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(f: F_Album) =>
        log.info("getAlbumData successful!!")
        sender ! albumRetrieved(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during getAlbumData?")

      case Failure(error) =>
        log.error(error, "Couldn't run getAlbumData :(")

    }

  case getPost(id, postId) =>
    val uri = Uri("http://localhost:8080/post") withQuery(F_Post.contentsString -> "",
                                                              F_Post.creatorString -> id.toString(16),
                                                              F_Post.locationType -> "",
                                                              F_Post.locationTypeString -> "",
                                                              F_Post.locationPage -> "",
                                                              F_Post.locationProfile -> "",
                                                              //F_Post.changableParamaters ->
                                                              F_Post.locationString -> postId.toString(16))
    log.info("=>> getPost sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Post]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(f: F_Post) =>
        log.info("getPost successful!!")
        sender ! postRetrieved(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during getPost?")

      case Failure(error) =>
        log.error(error, "Couldn't run getPost :(")

    }

  case getPageData(id) =>    //TODO no unique page id in page object in F_Page
    val uri = Uri("http://localhost:8080/page") withQuery(F_Page.joinPageString -> "",
                                                              F_Page.leavePageString -> "",
                                                              F_Page.newUserString -> "",
                                                              F_Page.nameString -> "",
                                                              F_Page.descriptionString -> "",
                                                              //F_Page.changableParameters -> ,
                                                              F_Page.ownerString -> "")
    log.info("=>> getPageData, sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Page]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(f: F_Page) =>
        log.info("getPageData successful!!")
        sender ! pageRetrieved(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during getPageData?")

      case Failure(error) =>
        log.error(error, "Couldn't run getPageData :(")

    }



  //Delete operations
  case deleteUser(id) =>
    val uri = Uri("http://localhost:8080/users") withQuery(F_User.lastNameString -> "",
                                                              F_User.firstNameString -> "",
                                                              F_User.bioString -> "",
                                                              F_User.dobString -> "",
                                                              F_User.ageString -> "",
                                                              // F_User.changableParameters -> ,
                                                              F_User.friendRequestString -> "",
                                                              F_User.acceptFriendString -> "",
                                                              F_User.friendRemoveString -> "")
    log.info("=>> deleteUser, sending request...")
    val pipeline = sendReceive ~> unmarshal[F_User]
    val responseFuture = pipeline {Delete(uri)}

    responseFuture onComplete {
      case Success(f: F_User) =>
        log.info("deleteUser successful!!")
        sender ! userDeleted(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during deleteUser?")

      case Failure(error) =>
        log.error(error, "Couldn't run deleteUser :(")

    }

  case deletePicture(userId, pId) =>        //TODO no way to identify picture without pic ID
    val uri = Uri("http://localhost:8080/picture") withQuery( F_Picture.ownerString -> userId.toString(16),
                                                                  F_Picture.nameString -> "",
                                                                  F_Picture.descriptionString -> "",
                                                                  //F_Picture.changableParameters -> ,
                                                                  F_Picture.albumString -> "")
    log.info("=>> deletePicture sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Picture]
    val responseFuture = pipeline {Delete(uri)}

    responseFuture onComplete {
      case Success(f: F_Picture) =>
        log.info("deletePicture successful!!")
        sender ! pictureDeleted(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during deletePicture?")

      case Failure(error) =>
        log.error(error, "Couldn't run deletePicture :(")

    }

  case deleteAlbum(userId, albmId) =>
    val uri = Uri("http://localhost:8080/album") withQuery( F_Album.ownerString -> userId.toString(16),
                                                                F_Album.nameString -> "",
                                                                //F_Album.changableParameters ->
                                                                F_Album.descriptionString -> "")
    log.info("=>> deleteAlbum sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Album]
    val responseFuture = pipeline {Delete(uri)}

    responseFuture onComplete {
      case Success(f: F_Album) =>
        log.info("deleteAlbum successful!!")
        sender ! albumDeleted(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during deleteAlbum?")

      case Failure(error) =>
        log.error(error, "Couldn't run deleteAlbum :(")

    }

  case deletePost(id, postId) =>
    val uri = Uri("http://localhost:8080/post") withQuery(F_Post.contentsString -> "",
                                                              F_Post.creatorString -> id.toString(16),
                                                              F_Post.locationType -> "",
                                                              F_Post.locationTypeString -> "",
                                                              F_Post.locationPage -> "",
                                                              F_Post.locationProfile -> "",
                                                              //F_Post.changableParamaters ->
                                                              F_Post.locationString -> postId.toString(16))
    log.info("=>> deletePost sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Post]
    val responseFuture = pipeline {Delete(uri)}

    responseFuture onComplete {
      case Success(f: F_Post) =>
        log.info("deletePost successful!!")
        sender ! postDeleted(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during deletePost?")

      case Failure(error) =>
        log.error(error, "Couldn't run deletePost :(")

    }

  case deletePage(id) =>        //TODO no user id in F_User objecr
    val uri = Uri("http://localhost:8080/page") withQuery(F_Page.joinPageString -> "",
                                                              F_Page.leavePageString -> "",
                                                              F_Page.newUserString -> "",
                                                              F_Page.nameString -> "",
                                                              F_Page.descriptionString -> "",
                                                              //F_Page.changableParameters -> ,
                                                              F_Page.ownerString -> "")
    log.info("=>> deletePage, sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Page]
    val responseFuture = pipeline {Delete(uri)}

    responseFuture onComplete {
      case Success(f: F_Page) =>
        log.info("deletePage successful!!")
        sender ! pageDeleted(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during deletePage?")

      case Failure(error) =>
        log.error(error, "Couldn't run deletePage :(")

    }


    //TODO how will the user know whom to send friend request, how will he know he received a request
  //TODO no userID in F_User object, how to send user identifier

  //Friend operations
  case sendFriendReq(id1, id2) =>
    val uri = Uri("http://localhost:8080/user/resuest") withQuery()  //send user id self and requested users
    log.info("=>> sendFriendRequest, sending request...")
    val pipeline = sendReceive ~> unmarshal[F_Page]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(f: F_Page) =>
        log.info("Page creation successful!!")
        sender ! pageCreated(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during Page creation?")

      case Failure(error) =>
        log.error(error, "Couldn't get Page Created :(")

    }

  case acceptFriendReq(id1, id2) =>
    //can be done at server as user doesn't have push notification about request
    //do we need this before authentication part??


  case _ =>
    log.info("case with no match received at BaseClient")

}


}
