package clientSim

//import scala.concurrent.Future
//import scala.concurrent.duration._
//import akka.pattern.ask
//import spray.can.Http
//import akka.io.IO
//import spray.json._
//import akka.util.Timeout
//import HttpMethods._
//import scala.concurrent._
//import scala.concurrent.ExecutionContext.Implicits.global._
import akka.actor._
import akka.actor.Actor
import akka.actor.ActorSystem
import spray.httpx.SprayJsonSupport
import spray.json.AdditionalFormats
import spray.json.{JsonFormat, DefaultJsonProtocol}
import spray.client.pipelining._
import spray.http._
import graphnodes._
import scala.util.{Success, Failure}
import system.jsonFiles.MyJsonProtocol._


class F_BaseClient extends Actor with ActorLogging with SprayJsonSupport with AdditionalFormats
{


  log.debug("Starting client side logs\n")

  case class createUser()
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
  case class getPictureData(id : BigInt)
  case class getAlbumData(id : BigInt)
  case class getPost(id : BigInt)
  case class getPageData(id :BigInt)
  case class deleteUser(id :BigInt)
  case class deletePicture(id :BigInt)
  case class deleteAlbum(Id :BigInt)
  case class deletePost( id : BigInt)
  case class deletePage(id :BigInt)
  case class sendFriendReq(id1 : BigInt, id2 :BigInt)
  case class acceptFriendReq(id1 :BigInt, id2 :BigInt)

  case class userCreated(res : F_User)
  case class postCreated(res : F_Post)
  case class pageCreated(res : F_Page)
  case class pictureUploaded(res : F_Picture)
  case class albumCreated(res : F_Album)
  case class profileCreated(res : F_UserProfile)

  case class userEdited(res : F_User)
  case class postEdited(res : F_Post)
  case class pageEdited(res : F_Page)
  case class pictureEdited(res : F_Picture)
  case class albumEdited(res : F_Album)
  case class profileEdited(res : F_UserProfile)

  case class userRetrieved(res : F_User)
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

  implicit val system = ActorSystem()
  import system.dispatcher

def receive = {


  //Create operations
  case createUser =>

    val uri = Uri("https://www.fakebook.com/newuser?") withQuery( F_User.lastNameString -> "",
                                                                  F_User.firstNameString -> "",
                                                                  F_User.bioString -> "",
                                                                  F_User.dobString -> "",
                                                                  F_User.ageString -> "",
                                                                 // F_User.changableParameters -> ,
                                                                  F_User.friendRequestString -> "",
                                                                  F_User.acceptFriendString -> "",
                                                                  F_User.friendRemoveString -> "")

    //val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
    // val response: Future[HttpResponse] = pipeline(Get(uri))
    val pipeline = sendReceive ~> unmarshal[F_User]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(f: F_User) =>
        log.info("createUser successful!!")
        sender ! userCreated(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during createUser?")

      case Failure(error) =>
        log.error(error, "Couldn't run createUser :(")

    }

  case createPost(id) =>
    val uri = Uri("https://www.fakebook.com/newpost?") withQuery( F_Post.contentsString -> "",
                                                                  F_Post.creatorString -> id.toString(16),
                                                                  F_Post.locationType -> "",
                                                                  F_Post.locationTypeString -> "",
                                                                  F_Post.locationPage -> "",
                                                                  F_Post.locationProfile -> "",
                                                                  //F_Post.changableParamaters -> ,
                                                                  F_Post.locationString -> "")
    val pipeline = sendReceive ~> unmarshal[F_Post]
    val responseFuture = pipeline {Put(uri)}
    responseFuture onComplete {
      case Success(f: F_Post) =>
        log.info("createPost successful!!")
        sender ! postCreated(f)

      case Success(somethingUnexpected) =>
        log.warning("Something unexpected during createPost?")

      case Failure(error) =>
        log.error(error, "Couldn't run createPost :(")

    }

  case createPage(id) =>
    val uri = Uri("https://www.fakebook.com/newpage?") withQuery( F_Page.joinPageString -> "",
                                                                  F_Page.leavePageString -> "",
                                                                  F_Page.newUserString -> "",
                                                                  F_Page.nameString -> "",
                                                                  F_Page.descriptionString -> "",
                                                                  //F_Page.changableParameters -> ,
                                                                  F_Page.ownerString -> id.toString(16))
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

  case createAlbum(id) =>
    val uri = Uri("https://www.fakebook.com/createalbum?") withQuery( F_Album.ownerString -> id.toString(16),
                                                                      F_Album.nameString -> "",
                                                                      //F_Album.changableParameters -> ,
                                                                      F_Album.descriptionString -> "")
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

  case uploadPicture(id) =>
    val uri = Uri("https://www.fakebook.com/data/uploadimage?") withQuery(F_Picture.ownerString -> id.toString(16),
                                                                          F_Picture.nameString -> "",
                                                                          F_Picture.descriptionString -> "",
                                                                          //F_Picture.changableParameters -> ,
                                                                          F_Picture.albumString -> "")

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
  case updateUserData(id) =>        //TODO user object doesn't have id string
    val uri = Uri("https://www.fakebook.com/user/request?") withQuery(F_User.lastNameString -> "",
                                                                      F_User.firstNameString -> "",
                                                                      F_User.bioString -> "",
                                                                      F_User.dobString -> "",
                                                                      F_User.ageString -> "",
                                                                      // F_User.changableParameters -> ,
                                                                      F_User.friendRequestString -> "",
                                                                      F_User.acceptFriendString -> "",
                                                                      F_User.friendRemoveString -> "")
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

  case updatePictureData(id) =>
    val uri = Uri("https://www.fakebook.com/picture?") withQuery( F_Picture.ownerString -> id.toString(16),
                                                                  F_Picture.nameString -> "",
                                                                  F_Picture.descriptionString -> "",
                                                                  //F_Picture.changableParameters -> ,
                                                                  F_Picture.albumString -> "")

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

  case updateAlbumData(id) =>
    val uri = Uri("https://www.fakebook.com/album?") withQuery( F_Album.ownerString -> id.toString(16),
                                                                F_Album.nameString -> "",
                                                                //F_Album.changableParameters ->
                                                                F_Album.descriptionString -> "")
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

  case updatePost(id) =>
    val uri = Uri("https://www.fakebook.com/post?") withQuery(F_Post.contentsString -> "",
                                                              F_Post.creatorString -> id.toString(16),
                                                              F_Post.locationType -> "",
                                                              F_Post.locationTypeString -> "",
                                                              F_Post.locationPage -> "",
                                                              F_Post.locationProfile -> "",
                                                              //F_Post.changableParamaters ->
                                                              F_Post.locationString -> "")
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

  case updatePageData(id) =>
    val uri = Uri("https://www.fakebook.com/page?") withQuery(F_Page.joinPageString -> "",
                                                              F_Page.leavePageString -> "",
                                                              F_Page.newUserString -> "",
                                                              F_Page.nameString -> "",
                                                              F_Page.descriptionString -> "",
                                                              //F_Page.changableParameters -> ,
                                                              F_Page.ownerString -> "")
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
  case getUserData(id) =>
    val uri = Uri("https://www.fakebook.com/user?") withQuery(F_User.lastNameString -> "",
                                                              F_User.firstNameString -> "",
                                                              F_User.bioString -> "",
                                                              F_User.dobString -> "",
                                                              F_User.ageString -> "",
                                                              // F_User.changableParameters -> ,
                                                              F_User.friendRequestString -> "",
                                                              F_User.acceptFriendString -> "",
                                                              F_User.friendRemoveString -> "")
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

  case getPictureData(id) =>
    val uri = Uri("https://www.fakebook.com/picture?") withQuery( F_Picture.ownerString -> id.toString(16),
                                                                  F_Picture.nameString -> "",
                                                                  F_Picture.descriptionString -> "",
                                                                  //F_Picture.changableParameters -> ,
                                                                  F_Picture.albumString -> "")
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

  case getAlbumData(id) =>
    val uri = Uri("https://www.fakebook.com/album?") withQuery( F_Album.ownerString -> id.toString(16),
                                                                F_Album.nameString -> "",
                                                                //F_Album.changableParameters ->
                                                                F_Album.descriptionString -> "")
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

  case getPost(id) =>
    val uri = Uri("https://www.fakebook.com/post?") withQuery(F_Post.contentsString -> "",
                                                              F_Post.creatorString -> id.toString(16),
                                                              F_Post.locationType -> "",
                                                              F_Post.locationTypeString -> "",
                                                              F_Post.locationPage -> "",
                                                              F_Post.locationProfile -> "",
                                                              //F_Post.changableParamaters ->
                                                              F_Post.locationString -> "")
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

  case getPageData(id) =>
    val uri = Uri("https://www.fakebook.com/page?") withQuery(F_Page.joinPageString -> "",
                                                              F_Page.leavePageString -> "",
                                                              F_Page.newUserString -> "",
                                                              F_Page.nameString -> "",
                                                              F_Page.descriptionString -> "",
                                                              //F_Page.changableParameters -> ,
                                                              F_Page.ownerString -> "")
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
    val uri = Uri("https://www.fakebook.com/user?") withQuery(F_User.lastNameString -> "",
                                                              F_User.firstNameString -> "",
                                                              F_User.bioString -> "",
                                                              F_User.dobString -> "",
                                                              F_User.ageString -> "",
                                                              // F_User.changableParameters -> ,
                                                              F_User.friendRequestString -> "",
                                                              F_User.acceptFriendString -> "",
                                                              F_User.friendRemoveString -> "")
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

  case deletePicture(id) =>
    val uri = Uri("https://www.fakebook.com/picture?") withQuery( F_Picture.ownerString -> id.toString(16),
                                                                  F_Picture.nameString -> "",
                                                                  F_Picture.descriptionString -> "",
                                                                  //F_Picture.changableParameters -> ,
                                                                  F_Picture.albumString -> "")
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

  case deleteAlbum(id) =>
    val uri = Uri("https://www.fakebook.com/album?") withQuery( F_Album.ownerString -> id.toString(16),
                                                                F_Album.nameString -> "",
                                                                //F_Album.changableParameters ->
                                                                F_Album.descriptionString -> "")
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

  case deletePost(id) =>
    val uri = Uri("https://www.fakebook.com/post?") withQuery(F_Post.contentsString -> "",
                                                              F_Post.creatorString -> id.toString(16),
                                                              F_Post.locationType -> "",
                                                              F_Post.locationTypeString -> "",
                                                              F_Post.locationPage -> "",
                                                              F_Post.locationProfile -> "",
                                                              //F_Post.changableParamaters ->
                                                              F_Post.locationString -> "")
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

  case deletePage(id) =>
    val uri = Uri("https://www.fakebook.com/page?") withQuery(F_Page.joinPageString -> "",
                                                              F_Page.leavePageString -> "",
                                                              F_Page.newUserString -> "",
                                                              F_Page.nameString -> "",
                                                              F_Page.descriptionString -> "",
                                                              //F_Page.changableParameters -> ,
                                                              F_Page.ownerString -> "")
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
    /*
  //Friend operations
  case sendFriendReq(id1, id2) =>
    val uri = Uri("https://www.fakebook.com/user/resuest?") withQuery(F_User.ID -> id1.toString(16))  //send user id self and requested users
  val pipeline = sendReceive ~> unmarshal[F_Page]
    val responseFuture = pipeline {
      Put(uri)
    }
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
    //do we need this before authentication part??

*/


}


}
