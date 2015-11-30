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


    log.info("=>> createUser sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(jsonUser) =>
        log.info("createUser successful!!")
        replyTo ! userCreated(jsonUser.parseJson.convertTo[F_User])

      case Failure(error) =>
        log.error(error, "Couldn't run createUser because of " + error.getMessage)

    }

  case createPost(posterId, content, locationType, locationId) =>
    val uri = Uri("http://localhost:8080/newpost") withQuery( F_Post.contentsString -> content,
                                                                  F_Post.creatorString -> posterId.toString(16),
                                                                  F_Post.locationString -> locationId.toString(16),
                                                                  F_Post.locationTypeString -> locationType)
    log.info("=>> createPost sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(jsonPost) =>
        log.info("createPost successful!!")
        replyTo ! postCreated(jsonPost.parseJson.convertTo[F_Post], locationType)

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
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("Page creation successful!!")
        replyTo ! pageCreated(jsonRef.parseJson.convertTo[F_Page])


      case Failure(error) =>
        log.error(error, "Couldn't get Page Created :(")

    }

  case createAlbum(userId, albmName, albmDes) =>
    val uri = Uri("http://localhost:8080/album/createalbum") withQuery( F_Album.ownerString -> userId.toString(16),
                                                                      F_Album.nameString -> albmName,
                                                                      //F_Album.changableParameters -> ,
                                                                      F_Album.descriptionString -> albmDes)
    log.info("=>> createAlbum sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("createAlbum successful!!")
        replyTo ! albumCreated(jsonRef.parseJson.convertTo[F_Album])

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
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("uploadPicture successful!!")
        replyTo ! pictureUploaded(jsonRef.parseJson.convertTo[F_Picture])


      case Failure(error) =>
        log.error(error, "Couldn't run uploadPicture :(")

    }



  //Update operations
  case updateUserData(id, fName, lName, bio) =>        //TODO user object doesn't have id string
    val uri = Uri("http://localhost:8080/users/request") withQuery(F_User.lastNameString -> lName,
                                                                      F_User.firstNameString -> fName,
                                                                      F_User.bioString -> bio,
                                                                      F_User.dobString -> "",
                                                                      F_User.ageString -> "")
                                                                      // F_User.changableParameters -> ,
                                                                      //F_User.friendRequestString -> "",
                                                                      //F_User.acceptFriendString -> "",
                                                                      //F_User.friendRemoveString -> "")
    log.info("=>> updateUserData, sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Post(uri)}
    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("updateUserData successful!!")
        replyTo ! userEdited(jsonRef.parseJson.convertTo[F_User])


      case Failure(error) =>
        log.error(error, "Couldn't run updateUserData :(")

    }


  case updateUserProfile(profId, des) =>      //TODO no identifier for profile either
    val uri = Uri("http://localhost:8080/profile") withQuery( F_UserProfile.profilePictureString -> "",
                                                                  // F_User.changableParameters -> ,
                                                                  F_UserProfile.descriptionString -> des)
    log.info("=>> updateUserProfile sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Post(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("updateUserData successful!!")
        replyTo ! userProfileEdited(jsonRef.parseJson.convertTo[F_UserProfile])


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
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Post(uri)}
    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("updatePictureData successful!!")
        replyTo ! pictureEdited(jsonRef.parseJson.convertTo[F_Picture])


      case Failure(error) =>
        log.error(error, "Couldn't run updatePictureData :(")

    }

  case updateAlbumData(userId, albmId, albmName, albmDes) =>
    val uri = Uri("http://localhost:8080/album") withQuery( F_Album.ownerString -> userId.toString(16),
                                                                F_Album.nameString -> albmName,
                                                                //F_Album.changableParameters ->
                                                                F_Album.descriptionString -> albmDes)
    log.info("=>> updateAlbumData, sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Post(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("updateAlbumData successful!!")
        replyTo ! albumEdited(jsonRef.parseJson.convertTo[F_Album])


      case Failure(error) =>
        log.error(error, "Couldn't run updateAlbumData :(")

    }

  case updatePost(id, postId, locationType, contents) =>
    val uri = Uri("http://localhost:8080/post") withQuery(F_Post.contentsString -> contents,
                                                              F_Post.creatorString -> id.toString(16),
                                                              F_Post.locationTypeString -> locationType,
                                                              F_Post.locationString -> postId.toString(16))
    log.info("=>> updatePost sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Post(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("updatePost successful!!")
        replyTo ! postEdited(jsonRef.parseJson.convertTo[F_Post])


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
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Post(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("updatePageData successful!!")
        replyTo ! pageEdited(jsonRef.parseJson.convertTo[F_Page])


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
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("getUserData successful!!")
        replyTo ! userRetrieved(jsonRef.parseJson.convertTo[F_User])


      case Failure(error) =>
        log.error(error, "Couldn't run getUserData :(")

    }


  case getUserProfile(id) =>
    val uri = Uri("http://localhost:8080/profile/" + id.toString(16))
    log.info("=>> getUserProfile sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("getUserData successful!!")
        replyTo ! userProfileRetrieved(jsonRef.parseJson.convertTo[F_UserProfile])


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
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("getPictureData successful!!")
        replyTo ! pictureRetrieved(jsonRef.parseJson.convertTo[F_Picture])


      case Failure(error) =>
        log.error(error, "Couldn't run getPictureData :(")

    }

  case getAlbumData(userId, albmId) =>
    val uri = Uri("http://localhost:8080/album") withQuery( F_Album.ownerString -> userId.toString(16),
                                                                F_Album.nameString -> "",
                                                                //F_Album.changableParameters ->
                                                                F_Album.descriptionString -> "")
    log.info("=>> getAlbumData, sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("getAlbumData successful!!")
        replyTo ! albumRetrieved(jsonRef.parseJson.convertTo[F_Album])


      case Failure(error) =>
        log.error(error, "Couldn't run getAlbumData :(")

    }

  case getPost(id, postId) =>
    val uri = Uri("http://localhost:8080/post")

    log.info("=>> getPost sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("getPost successful!!")
        replyTo ! postRetrieved(jsonRef.parseJson.convertTo[F_Post])


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
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Get(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("getPageData successful!!")
        replyTo ! pageRetrieved(jsonRef.parseJson.convertTo[F_Page])


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
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Delete(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("deleteUser successful!!")
        replyTo ! userDeleted(jsonRef.parseJson.convertTo[F_User])


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
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Delete(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("deletePicture successful!!")
        replyTo ! pictureDeleted(jsonRef.parseJson.convertTo[F_Picture])


      case Failure(error) =>
        log.error(error, "Couldn't run deletePicture :(")

    }

  case deleteAlbum(userId, albmId) =>
    val uri = Uri("http://localhost:8080/album") withQuery( F_Album.ownerString -> userId.toString(16),
                                                                F_Album.nameString -> "",
                                                                //F_Album.changableParameters ->
                                                                F_Album.descriptionString -> "")
    log.info("=>> deleteAlbum sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Delete(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("deleteAlbum successful!!")
        replyTo ! albumDeleted(jsonRef.parseJson.convertTo[F_Album])


      case Failure(error) =>
        log.error(error, "Couldn't run deleteAlbum :(")

    }

  case deletePost(id, postId) =>
    val uri = Uri("http://localhost:8080/post")
    log.info("=>> deletePost sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Delete(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("deletePost successful!!")
        replyTo ! postDeleted(jsonRef.parseJson.convertTo[F_Post])


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
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Delete(uri)}

    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("deletePage successful!!")
        replyTo ! pageDeleted(jsonRef.parseJson.convertTo[F_Page])


      case Failure(error) =>
        log.error(error, "Couldn't run deletePage :(")

    }


    //TODO how will the user know whom to send friend request, how will he know he received a request
  //TODO no userID in F_User object, how to send user identifier

  //Friend operations
  case sendFriendReq(id1, id2) =>
    val uri = Uri("http://localhost:8080/user/resuest") withQuery()  //send user id self and requested users
    log.info("=>> sendFriendRequest, sending request...")
    var replyTo = sender
    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(jsonRef) =>
        log.info("friend request successful!!")
        //replyTo ! pageCreated(jsonRef)


      case Failure(error) =>
        log.error(error, "Couldn't send friend request :(")

    }

  case acceptFriendReq(id1, id2) =>
    //can be done at server as user doesn't have push notification about request
    //do we need this before authentication part??


  case _ =>
    log.info("case with no match received at BaseClient")

}


}
