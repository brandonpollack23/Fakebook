package clientSim

import java.security._
import java.util.Date
import javax.crypto.{KeyGenerator, SecretKey}
import akka.actor.{Actor, _}
import clientSim.CaseObjects._
import graphnodes._
import spray.client.pipelining.{sendReceive, _}
import spray.http.HttpHeaders.Cookie
import spray.http._
import spray.json._
import util.MyJsonProtocol._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.concurrent.duration._
import language.postfixOps
import scala.util.Random
import graphnodes.F_User
import util.Crypto._



class F_UserClient(clientRest: Int) extends Actor with ActorLogging {

/*
 * Code block 1
 * User Profile and state-data record keeping
 *
 * */

  //Match strings for function arguments
  val userType      : String = "user"
  val profileType   : String = "profile"
  val postType      : String = "post"
  val pageType      : String = "page"
  val picType       : String = "picture"
  val albumType     : String = "album"
  val friendRequest : String = "friendRequest"
  val handleRequest  : String = "handleRequest"
  val removeFriend  : String = "removeFriend"
  val getUserList   : String = "getUserList"
  val friendUserInfo: String = "friendUserInfo"
  val friendRequesterInfo :String = "friendRequesterInfo"

  //AES Encryption
  val kGen: KeyGenerator = KeyGenerator.getInstance("AES")
  kGen.init(128)
  val aesKey: SecretKey = kGen.generateKey

  //RSA Encryption
  val kpg: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
  kpg.initialize(2048)
  val kp: KeyPair = kpg.genKeyPair
  val publicKey  = kp.getPublic
  val privateKey = kp.getPrivate


  //default profiles for Put requests
  var user_ME    = F_User("Ali", "Gator", "Student at UF", 25, new Date(1989-1900,1,1), new Date(2015-1900,1,1), List[(BigInt, SecretKey)](), List[(BigInt, Array[Byte])](), 0, publicKey, 0)
  var profile_ME = F_UserProfile(List[BigInt](), new Date(2015-1900,1,1), 0, List[BigInt](), 0, "My Profile", 0, 0)
  var page_ME    = F_Page("Gator Times", "All about UF", new Date(2015-1900,1,1), List[BigInt](), List[BigInt](), 0, List[BigInt](), 0, 0, 0)
  var post_ME   = F_Post("Some News", 0, "profile", 0, new Date(2015-1900,1,1), 0)
  var album_ME :F_Album   = F_Album("My Album", "Vacations", new Date(2015-1900,1,1), false, 0, 0, List[BigInt]())
  var pic_ME :F_Picture    = F_Picture("My Pic", "Holidays", 0, new Date(2015-1900,1,1), 0, 0, 0)
  var friendUser : F_UserE = F_UserE(null,null,null,null,null,null,null,null,0,null,0)

  //Lists to record current status
  var myPosts  = List[F_Post]()
  var myPagePosts = List[BigInt]()
  var myProfPosts = List[BigInt]()
  var myPics   = List[F_Picture]()
  var myAlbums = List[BigInt]()
  var myPages  = List[BigInt]()
  var allUsers = List[BigInt]()

  var myAuthCookie: HttpHeader = Cookie(HttpCookie(F_User.authenticationCookieName, content = "0"))




  //#Works
  def authRequest = {

    val uri = Uri("http://localhost:8080/users/auth/"+user_ME.userID.toString(16))

    val pipeline = sendReceive
    val responseFuture = pipeline {Post(uri)}

    responseFuture onComplete {

      case Success(authenticationResponse) =>
        if(authenticationResponse.headers.isEmpty) throw new Exception("No cookie in auth reply!")
        val cookies = authenticationResponse.headers.collect {
          case Cookie(ck) => ck.find(_.name == F_User.authenticationCookieName)
        }
        val authenticationCookie = cookies.head.get

        val encryptedCode = BigInt(authenticationCookie.content, 16).toByteArray
        val decryptedCodeString = BigInt(encryptedCode.decryptRSA(privateKey)).toString(16)
        myAuthCookie = Cookie(HttpCookie(F_User.authenticationCookieName, content = decryptedCodeString))
        self! Authenticated

      case Failure(error) =>
        log.error(error, "Failed to fetch authorization cookie " + error.getMessage)

    }

  }


  def putRequest(reqType:String, aUser:F_User=null, aPage:F_Page=null, aPost:F_Post=null, aAlbum:F_Album=null, aPic:F_Picture=null) = reqType match{

    //#Works
    case "user" =>

      val uri = Uri("http://localhost:8080/users/newuser")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, aUser.encryptUser(aesKey).toJson.compactPrint))}
      //log.info("==============>>>>>>>> create : user")
      responseFuture onComplete {

        case Success(jsonRef) =>
          user_ME = jsonRef.parseJson.convertTo[F_UserE].decryptUserE(aesKey,privateKey)
          //log.info("==============>>>>>>>> user profile created")
          self ! UserCreated

        case Failure(error) =>
          log.error(error, "Failed to run Create User because of " + error.getMessage)
      }

    //#Works
    case "page" =>

      val uri = Uri("http://localhost:8080/page/newpage") withQuery(F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, aPage.toJson.compactPrint)) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> create : page")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("==============>>>>>>>> Page creation successful!!")
          myPages ::= jsonRef.parseJson.convertTo[F_Page].ID
          self ! Simulate//#
          //self ! PageCreated

        case Failure(error) =>
          log.error(error, "Couldn't get Page Created !!")
          self ! Simulate//#
      }

    //#Works
    case "post" =>
      val location = aPost.locationType

      val uri = Uri("http://localhost:8080/post") withQuery(F_User.ownerQuery -> user_ME.userID.toString(16), F_Post.locationTypeString -> location)
      //log.info("==============>>>>>>>> create : post")
      val pipeline = sendReceive
      val responseFuture = if(location == F_Post.locationProfile) pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, aPost.encryptPost(aesKey).toJson.compactPrint)) withHeaders myAuthCookie}
                            else pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, aPost.toJson.compactPrint)) withHeaders myAuthCookie}

      responseFuture onComplete {

        case Success(response) =>
          //log.info("==============>>>>>>>> createPost successful!!")
          val temp = if(location == F_Post.locationProfile) response.entity.asString.parseJson.convertTo[F_PostE].decryptPost(aesKey)
                                  else response.entity.asString.parseJson.convertTo[F_Post]
          myPosts ::= temp
         /* if(temp.locationType=="profile")
            myProfPosts ::= temp.postID
          else{
            myPagePosts ::= temp.postID
          }*/
          self ! Simulate//#
          //self ! PostCreated

        case Failure(error) =>
          log.error(error, "Couldn't run createPost  !!")
          self ! Simulate//#
      }

    //#Works
    case "album" =>

      val uri = Uri("http://localhost:8080/album/createalbum") withQuery(F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, aAlbum.encryptAlbum(aesKey).toJson.compactPrint)) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> create : album")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("============>>>>>>>>>>> Album successfully Created !!")
          myAlbums ::= jsonRef.parseJson.convertTo[F_AlbumE].decryptAlbumE(aesKey).id
          self ! Simulate//#
          //self ! AlbumCreated

        case Failure(error) =>
          log.error(error, "Couldn't run createAlbum  !!")
          self ! Simulate//#
      }

    //#Works
    case "picture" =>

      val uri = Uri("http://localhost:8080/data/uploadimage") withQuery(F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val picTrans:F_PictureTransmit = F_PictureTransmit(aPic.encryptPicture(aesKey), Array[Byte](1,2,3))
      val responseFuture = pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, picTrans.toJson.compactPrint)) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> create : picture")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("uploadPicture successful!!")
          myPics ::= jsonRef.parseJson.convertTo[F_PictureE].decryptPictureE(aesKey)
          self ! Simulate//#
          //self ! PictureUploaded

        case Failure(error) =>
          log.error(error, "Couldn't run uploadPicture !!")
          self ! Simulate//#
      }

  }


  def getRequest(reqType:String,userId:BigInt=0, profileId:BigInt=0, pageId:BigInt=0, postId:BigInt=0, albumId:BigInt=0, picId:BigInt=0) = reqType match {

    //#Works
    case "user" =>

      val uri = Uri("http://localhost:8080/users/"+userId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}
      //log.info("==============>>>>>>>> get : user")
      responseFuture onComplete {
        case Success(jsonRef) =>
          //log.info("==============>>>>>>>> User Data successfully Retrieved!!")
          user_ME = jsonRef.parseJson.convertTo[F_UserE].decryptUserE(aesKey, privateKey)
          if(user_ME.friendRequests.isEmpty)
            self ! Simulate
          //self ! UserDataRetrieved
          else
            self ! HandleFriendRequest

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve User Data !!")
          self ! Simulate//#
      }

    //#Works
    case "profile" =>

      val uri = Uri("http://localhost:8080/profile/" + profileId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}
      //log.info("==============>>>>>>>> get : profile")
      responseFuture onComplete {

        case Success(jsonRef) =>
          profile_ME = jsonRef.parseJson.convertTo[F_UserProfileE].decryptUserProfileE(aesKey)
          //log.info("==============>>>>>>>> user Profile Retrieved successfully")
          //self ! ProfileRetrieved
          self ! Simulate

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve User Profile !!")
          self ! Simulate//#
      }

    //#Works
    case "page" =>

      val uri = Uri("http://localhost:8080/page/" + pageId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}
      //log.info("==============>>>>>>>> get : page")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("Page Data successfully Retrieved !!")
          self ! Simulate//#
          //self ! PageRetrieved

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve Page Data !!")
          self ! Simulate//#
      }

    //#Works
    case "post" =>

      val uri = Uri("http://localhost:8080/post/"+postId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}
      //log.info("==============>>>>>>>> get : post")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("==============>>>>>>>> Post successfully Retrieved !!")
          self ! Simulate//#
          //self ! PostRetrieved

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve Post !!")
          self ! Simulate//#
      }

    //#Works
    case "album" =>

      val uri = Uri("http://localhost:8080/album/"+albumId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}
      //log.info("==============>>>>>>>> get : album")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("Album data successfully Retrieved !!")
          self ! Simulate//#
          //self ! AlbumRetrieved

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve Album Data !!")
          self ! Simulate//#
      }

    //#Works
    case "picture" =>

      val uri = Uri("http://localhost:8080/picture/"+picId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}
      //log.info("==============>>>>>>>> get : picture")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("get Picture Data successful!!")
          self ! Simulate//#
          //self ! PictureRetrieved

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve Picture Data !!")
          self ! Simulate//#
      }

    //#Works
    case "getUserList" =>
      val uri = Uri("http://localhost:8080/users/getall")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("Received List of all users !!")
          allUsers = jsonRef.parseJson.convertTo[List[BigInt]]
          self ! UserListRetrieved
        //self ! Simulate//#


        case Failure(error) =>
          log.error(error, "Couldn't Retrieve User List !!")
        //self ! Simulate//#
      }

    //#Works
    case "friendUserInfo" =>

      val uri = Uri("http://localhost:8080/users/"+userId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}

      responseFuture onComplete {
        case Success(jsonRef) =>
          //log.info("==============>>>>>>>> User Data successfully Retrieved!!")
          friendUser = jsonRef.parseJson.convertTo[F_UserE]
          //self ! Simulate//#
          self ! FriendUserInfoRetrieved

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve User Data !!")
          self ! Simulate//#
      }

    //#works
    case "friendRequesterInfo" =>

      val uri = Uri("http://localhost:8080/users/"+userId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}

      responseFuture onComplete {
        case Success(jsonRef) =>
          //log.info("==============>>>>>>>> Friend request sender data successfully Retrieved!!")
          friendUser = jsonRef.parseJson.convertTo[F_UserE]
          //self ! Simulate//#
          self ! FriendRequesterInfoRetrieved

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve Friend request sender Data !!")
        //self ! Simulate//#
      }

  }


  def postRequest(reqType:String, aUser:F_User=null, aProfile:F_UserProfile=null, aPage:F_Page=null, aPost:F_Post=null, aAlbum:F_Album=null, aPic:F_Picture=null, acc:Boolean=true) = reqType match {

    //#Works
    case "user" =>

      val uri = Uri("http://localhost:8080/users/"+user_ME.userID.toString(16)) withQuery(F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aUser.encryptUser(aesKey).toJson.compactPrint)) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> post : user")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("updateUserData successful!!")
          user_ME = jsonRef.parseJson.convertTo[F_UserE].decryptUserE(aesKey, privateKey)
          self ! Simulate//#
          //self ! UserUpdated

        case Failure(error) =>
          log.error(error, "Couldn't run updateUserData  !!")
          self ! Simulate//#
      }

    //#Works
    case "profile" =>

      val uri = Uri("http://localhost:8080/profile/" + user_ME.profileID.toString(16)) withQuery (F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aProfile.encryptUserProfile(aesKey).toJson.compactPrint)) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> post : profile")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("updateUserData successful!!")
          profile_ME = jsonRef.parseJson.convertTo[F_UserProfileE].decryptUserProfileE(aesKey)
          self ! Simulate//#
          //self ! ProfileUpdated

        case Failure(error) =>
          log.error(error, "Couldn't run updateUserProfile !!")
          self ! Simulate//#
      }

    //#Works
    case "page" =>

      val uri = Uri("http://localhost:8080/page/"+aPage.ID.toString(16)) withQuery (F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aPage.toJson.compactPrint)) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> post : page")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info(" ====>>>>>  Update Page Data successful !!")
          self ! Simulate//#
          //self ! PageUpdated

        case Failure(error) =>
          log.error(error, "Couldn't run Update Page Data !!")
          self ! Simulate//#
      }

    //#Works
    case "post" =>
      val location = aPost.locationType

      val uri = Uri("http://localhost:8080/post/"+aPost.postID.toString(16)) withQuery(F_User.ownerQuery -> user_ME.userID.toString(16))
      //log.info("==============>>>>>>>> post : post")
      val pipeline = sendReceive
      val responseFuture = if(location == F_Post.locationProfile) pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aPost.encryptPost(aesKey).toJson.compactPrint)) withHeaders myAuthCookie}
      else pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aPost.toJson.compactPrint)) withHeaders myAuthCookie}

      /*val uri = Uri("http://localhost:8080/post/" + aPost.postID.toString(16)) withQuery (F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aPost.encryptPost(aesKey).toJson.compactPrint)) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> post : post")
      */responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("===>>>>  update Post successful!!")
          self ! Simulate//#
          //self ! PostUpdated

        case Failure(error) =>
          log.error(error, "Couldn't run update Post  !!")
          self ! Simulate//#
      }

    //#Works
    case "album" =>

      val uri = Uri("http://localhost:8080/album/"+ aAlbum.id.toString(16)) withQuery (F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aAlbum.encryptAlbum(aesKey).toJson.compactPrint)) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> post : album")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("updateAlbumData successful!!")
          self ! Simulate//#
          //self ! AlbumUpdated

        case Failure(error) =>
          log.error(error, "Couldn't run updateAlbumData :(")
          self ! Simulate//#
      }

    //#Works
    case "picture" =>

      val uri = Uri("http://localhost:8080/picture/"+ aPic.pictureID.toString(16)) withQuery (F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aPic.encryptPicture(aesKey).toJson.compactPrint)) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> post : picture")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("updatePictureData successful!!")
          self ! Simulate//#
          //self ! PictureUpdated

        case Failure(error) =>
          log.error(error, "Couldn't run updatePictureData :(")
          self ! Simulate//#
      }

    //#Works
    case "friendRequest" =>
      val uri = Uri("http://localhost:8080/users/request/"+user_ME.userID.toString(16)) withQuery(F_User.ownerQuery -> user_ME.userID.toString(16) ,F_User.friendRequestString -> friendUser.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aesKey.toByteArray.encryptRSA(friendUser.identityKey).toJson.compactPrint)) withHeaders myAuthCookie}

      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("Friend request sent !!")
          //self ! Simulate//#
          self ! FriendRequestSent

        case Failure(error) =>
          log.error(error, "Couldn't send Friend request !!")
          self ! Simulate//#
      }

    //#works
    case "handleRequest" => // do for all friend requests in the list, have a probability of not accepting, check if friendRequests.isEmpty before doing anything
      val requestingFriend = user_ME.friendRequests.head
      val friendUri = Uri("http://localhost:8080/users/" + requestingFriend._1.toString(16))

      val getFriendPipeline = sendReceive ~> unmarshal[String]
      val requestingFriendObjectIdentityKey = Await.result(getFriendPipeline(Get(friendUri)), 5 seconds).parseJson.convertTo[F_UserE].identityKey

      val uri = Uri("http://localhost:8080/users/request/handle/"+user_ME.userID.toString(16)) withQuery(F_User.ownerQuery -> user_ME.userID.toString(16) ,F_User.friendRequestString -> user_ME.friendRequests.head._1.toString(16), F_User.acceptFriendString -> acc.toString)

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aesKey.toByteArray.encryptRSA(requestingFriendObjectIdentityKey).toJson.compactPrint)) withHeaders myAuthCookie}

      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("=======>>>>  Friend request handled !!")
          //self ! Simulate//#
          self ! FriendRequestHandled

        case Failure(error) =>
          log.error(error, "Couldn't handle Friend request !!")
          self ! Simulate//#
      }

    //#Works
    case "removeFriend" =>
      val uri = Uri("http://localhost:8080/users/remove/"+user_ME.userID.toString(16)) withQuery(F_User.ownerQuery -> user_ME.userID.toString(16), F_User.friendRemoveString -> user_ME.friends.head._1.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri) withHeaders myAuthCookie}

      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("===>>>   Friend removed successfully !!")
          self ! Simulate//#


        case Failure(error) =>
          log.error(error, "Couldn't remove Friend !!")
          self ! Simulate//#
      }

  }


  def deleteRequest(reqType:String, pageId:BigInt=0, aPost:F_Post=null, albumId:BigInt=0, aPic:F_Picture=null) = reqType match {

    //#Works
    case "user" =>
      val uri = Uri("http://localhost:8080/users/"+user_ME.userID.toString(16)) withQuery(F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Delete(uri) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> delete : user")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("User deleted successfully !!")
          self ! UserDeleted

        case Failure(error) =>
          log.error(error, "Couldn't delete User !!")

      }

    //#Works
    case "page" =>

      val uri = Uri("http://localhost:8080/page/"+pageId.toString(16)) withQuery(F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Delete(uri) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> delete : page")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info(" Delete Page successful !!")
          myPages = myPages.filter(_!=pageId)
          self ! Simulate//#
          //self ! PageDeleted

        case Failure(error) =>
          log.error(error, "Couldn't run Delete Page !!")
          self ! Simulate//#
      }

    //#Works
    case "post" =>

      val uri = Uri("http://localhost:8080/post/"+aPost.postID.toString(16)) withQuery(F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Delete(uri) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> delete : post")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("Delete Post successful !!")
          myPosts = myPosts.filter(_!=aPost)
          //myPagePosts = myPagePosts.filter(_!=postId)
          //myProfPosts = myProfPosts.filter(_!=postId)
          self ! Simulate//#
          //self ! PostDeleted

        case Failure(error) =>
          log.error(error, "Couldn't run deletePost !! ")
          self ! Simulate//#
      }

    //#Works
    case "album" =>

      val uri = Uri("http://localhost:8080/album/"+albumId.toString(16)) withQuery(F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Delete(uri) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> delete : album")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("deleteAlbum successful!!")
          myAlbums = myAlbums.filter(_!=albumId)
          self ! Simulate//#
          //self ! AlbumDeleted

        case Failure(error) =>
          log.error(error, "Couldn't run deleteAlbum :(")
          self ! Simulate//#
      }

    //#Works
    case "picture" =>

      val uri = Uri("http://localhost:8080/picture/"+aPic.pictureID.toString(16)) withQuery(F_User.ownerQuery -> user_ME.userID.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Delete(uri) withHeaders myAuthCookie}
      //log.info("==============>>>>>>>> delete : picture")
      responseFuture onComplete {

        case Success(jsonRef) =>
          //log.info("delete Picture successful!!")
          myPics = myPics.filter(_!=aPic)
          self ! Simulate//#
          //self ! PictureDeleted

        case Failure(error) =>
          log.error(error, "Couldn't run deletePicture  !!")
          self ! Simulate//#
      }


  }


  def receive ={


    //original Simulation block begins

        case Begin =>
          putRequest(userType, user_ME)

        case UserCreated =>
          authRequest

        case Authenticated =>
          getRequest(profileType, profileId = user_ME.profileID)

        case ProfileRetrieved =>
          self ! Simulate

        case HandleFriendRequest =>
          getRequest(friendRequesterInfo, userId = user_ME.friendRequests.head._1)
          //self ! Simulate

        case FriendRequesterInfoRetrieved =>
          val r = Random.nextInt(100)
          if(r<90) {
            postRequest(handleRequest, acc = true)
          }
          else {
            postRequest(handleRequest, acc = false)
          }

        case FriendRequestHandled =>
          //getRequest(userType, userId= user_ME.userID)
          self ! Simulate

        case UserListRetrieved =>
          //log.info("============>>>>>>>>>>>>>>> User List retrieved successful !!")
          if(allUsers.length>1) {
            var i = Random.nextInt(allUsers.length)
            while(allUsers(i) == user_ME.userID){
              i = Random.nextInt(allUsers.length)
            }
            getRequest(friendUserInfo, userId = allUsers(i))
          }

        case FriendUserInfoRetrieved =>
          //log.info("============>>>>>>>>>>>>>>> Friend User info retrieved successful !!")
          postRequest(friendRequest)

        case FriendRequestSent =>
          //log.info("============>>>>>>>>>>>>>>> Friend Request Sent successfully !!")
          self ! Simulate


        case Simulate =>
          Thread.sleep(clientRest)

          val x = Random.nextInt(100)
          //println("x= ------>  "+x)
          //do some Post related activity  30% of Time
          if(x<35)
          {
            val y = Random.nextInt(100)
            //println("y=----->  "+y)
            if (y < 25) {
              putRequest(postType, aPost = post_ME.copy(creator=user_ME.userID, locationType= "profile" ,location = user_ME.profileID))
            }
            if(y>=25 && y<50) {
              if (myPages.nonEmpty)
                putRequest(postType, aPost = post_ME.copy(creator=user_ME.userID, locationType="page", location= myPages.head))
              else
                self ! Simulate
            }
            if (y >= 50 && y < 75) {
              if (myPosts.nonEmpty)
                getRequest(postType, postId = myPosts.head.postID)
              else
                self ! Simulate
            }
            if (y >= 75 && y < 90) {
              val z = Random.nextInt(100)
              //println("z= -->>>> "+z)
              if(z<100){
              if (myPosts.nonEmpty)
                postRequest(postType, aPost = myPosts.head.copy(contents = "updated contents"))
              else
                self ! Simulate
              }
            else{
              if(myProfPosts.nonEmpty)
                postRequest(postType, aPost = post_ME.copy(contents="new contents",creator=user_ME.userID, locationType="profile",location=myProfPosts.head))
              else
                self ! Simulate
              }
            }
            if(y>=90) {
              if (myPosts.length>1)
                deleteRequest(postType, aPost = myPosts.head)
              else
                self ! Simulate
            }
          }



          //do some picture/album activity 30 percent
          if(x>=35 && x<70) //Picture
          {
            val y = Random.nextInt(100)
            if(y<=70) {
              val z = Random.nextInt(100)
              if (z <= 50) {
               // if(myAlbums.nonEmpty)
                //  putRequest(picType, aPic = pic_ME.copy(containingAlbum=myAlbums.head, ownerID = user_ME.userID))
                //else
                  putRequest(picType, aPic = pic_ME.copy(containingAlbum=profile_ME.defaultAlbum, ownerID = user_ME.userID))
              }
              if (z > 50 && z < 75){
                if(myPics.nonEmpty){
                  getRequest(picType, picId = myPics.head.pictureID)
                }
                else
                  self ! Simulate
              }
              if (z >= 75 && z < 90){
                if(myPics.nonEmpty) {
                    postRequest(picType, aPic = myPics.head.copy(name="new pic name"))
                }
                else
                  self ! Simulate
              }
                if(z>=90){
                if(myPics.length>1) {
                    deleteRequest(picType, aPic = myPics.head)
                }
                else
                  self ! Simulate
                }
            }
            else {    //Album
              val z = Random.nextInt(100)
              if (z <= 50) {
                putRequest(albumType, aAlbum = album_ME.copy(ownerID=user_ME.userID))
              }
              if (z > 50 && z < 75) {
                if(myAlbums.nonEmpty)
                  getRequest(albumType, albumId = myAlbums.head)
                else
                  self ! Simulate
              }
              if (z >= 75 && z < 90) {
                if(myAlbums.nonEmpty)
                  postRequest(albumType, aAlbum = album_ME.copy(name="album updated", ownerID=user_ME.userID, id=myAlbums.head))
                else
                  self ! Simulate
              }
              if(z >= 90) {
                //if(myAlbums.length>1)
                //  deleteRequest(albumType, albumId = myAlbums.head)
                //else
                  self ! Simulate
              }
            }
          }


          //user data and profile data activity  15% of Time
          if(x>=70 && x< 80)
          {
            val y = Random.nextInt(100)
            if (y <= 25)
              postRequest(userType, aUser = user_ME.copy(firstName="new name",age=26))
            if (y > 25 && y< 50)
              getRequest(userType, userId= user_ME.userID)
            if(y>=50 && y< 75)
              postRequest(profileType, aProfile = profile_ME.copy(description="new job"))
            if(y>=75)
              getRequest(profileType, profileId= user_ME.profileID)

          }


          if(x>=80 && x< 95)//do something about page  15%
          {
            val y = Random.nextInt(100)
            if (y <= 50) {
              putRequest(pageType, aPage = page_ME.copy(ownerID = user_ME.userID))
            }
            if (y > 50 && y < 75) {
              if (myPages.nonEmpty){
                getRequest(pageType, pageId=myPages.head)
              }
              else
                self ! Simulate
            }
            if (y >= 75 && y < 90) {
              if (myPages.nonEmpty){
                postRequest(pageType, aPage = page_ME.copy(name = "new page name", ownerID = user_ME.userID, ID = myPages.head))
              }
              else
                self ! Simulate
            }
            if(y >= 90) {
              //if (myPages.length > 1) {
                //deleteRequest(pageType, pageId = myPages.head)
              //}
              //else
                self ! Simulate
            }

          }


          if(x>=95)//do something about friendList - 10%
          {
            val r = Random.nextInt(100)
            if(r<90) {
              //send friend request
              getRequest(getUserList)
            }
            else{
            //remove some friend
              if(user_ME.friends.nonEmpty)
                postRequest(removeFriend)
              else
                self ! Simulate
            }
          }



    //Simulation block ends


    /*
    //Test code block begins
    case Begin =>
      putRequest(userType,user_ME)

    case UserCreated =>
      //log.info("============>>>>>>>>>>>>>>>User creation successful !!")
      authRequest

    case Authenticated =>
      getRequest(profileType,profileId = user_ME.profileID)

    case ProfileRetrieved => //Put any test request under this case and steer match forward as per requirement
        //putRequest(picType, aPic=pic_ME.copy(ownerID=user_ME.userID,containingAlbum=profile_ME.defaultAlbum))
       //putRequest(pageType, aPage= page_ME.copy(ownerID=user_ME.userID))
       // putRequest(postType, aPost= post_ME.copy(creator=user_ME.userID,locationType="profile", location=user_ME.profileID))
       //putRequest(albumType, aAlbum=album_ME.copy(ownerID=user_ME.userID))
        Thread.sleep(5000)
      getRequest(getUserList)   //1. get all user list

    case UserListRetrieved =>     //2. get user info of friend
      //log.info("============>>>>>>>>>>>>>>>User List retrieved successful !!")
      if(allUsers.length>1) {
        var i = 0
        while(allUsers(i) == user_ME.userID){
          i += 1
        }
        getRequest(friendUserInfo, userId = allUsers(i))
      }

    case FriendUserInfoRetrieved =>     //3. send friend request
      //log.info("============>>>>>>>>>>>>>>> Friend User info retrieved successful !!")
      postRequest(friendRequest)

    case FriendRequestSent =>
      //log.info("============>>>>>>>>>>>>>>> Friend Request Sent successfully !!")
      getRequest(userType, userId = user_ME.userID)

    case HandleFriendRequest =>
      Thread.sleep(5000)
      getRequest(friendRequesterInfo, userId = user_ME.friendRequests.head._1)

    case FriendRequesterInfoRetrieved =>
      postRequest(handleRequest)

    case FriendRequestHandled =>
      postRequest(removeFriend)


    case PictureUploaded =>
      //log.info("============>>>>>>>>>>>>>>> Picture upload successful !!")
       //deleteRequest(picType, picId = myPics.head)
       getRequest(picType, picId =myPics.head)
       //postRequest(picType, aPic= pic_ME.copy(name="new name", ownerID=user_ME.userID, pictureID=myPics.head))


    case PageCreated =>
      //log.info("============>>>>>>>>>>>>>>> Page creation successful !!")
       //deleteRequest(pageType, pageId=myPages.head)
       //postRequest(pageType, aPage= page_ME.copy(name="new name", ownerID=user_ME.userID,ID=myPages.head))
      //deleteRequest(pageType, pageId=myPages.head)
      //postRequest(pageType, aPage= page_ME.copy(name="new name", ownerID=user_ME.userID,ID=myPages.head))
      //getRequest(pageType,pageId=myPages.head)
      //putRequest(postType, aPost= post_ME.copy(creator=user_ME.userID,locationType="page", location=myPages.head))
      putRequest(postType, aPost= post_ME.copy(creator=user_ME.userID,locationType="page", location=myPages.head))

    case PageRetrieved =>
      //log.info("============>>>>>>>>>>>>>>> Page Retrival successful !!")
      postRequest(pageType, aPage= page_ME.copy(name="new name", ownerID=user_ME.userID,ID=myPages.head))

    case PageUpdated =>
      //log.info("============>>>>>>>>>>>>>>> Page updation successful !!")
      deleteRequest(pageType, pageId=myPages.head)

    case PostCreated =>
      //log.info("============>>>>>>>>>>>>>>> Post creation successful !!")
      postRequest(postType, aPost = post_ME.copy(contents ="new changes", creator=user_ME.userID,locationType="profile", location=user_ME.profileID,postID=myPosts.head))

    case PostUpdated =>
      //log.info("============>>>>>>>>>>>>>>> Post updation successful !!")
      getRequest(postType, postId= myPosts.head)

    case PostRetrieved =>
      //log.info("============>>>>>>>>>>>>>>> Post retrieval successful !!")
      deleteRequest(postType, postId= myPosts.head)

    case PostDeleted =>
      //log.info("============>>>>>>>>>>>>>>> Post deletion successful !!")

    case AlbumCreated =>
      //log.info("============>>>>>>>>>>>>>>> Album creation successful !!")
      getRequest(albumType, albumId= myAlbums.head)

    case AlbumRetrieved =>
      //log.info("============>>>>>>>>>>>>>>> Album retrieval successful !!")
      postRequest(albumType,  aAlbum= album_ME.copy(name="new name", ownerID=user_ME.userID, id=myAlbums.head))

    case AlbumUpdated =>
      //log.info("============>>>>>>>>>>>>>>> Album retrieval successful !!")
      deleteRequest(albumType, albumId=myAlbums.head)

    case PictureRetrieved =>
      //log.info("============>>>>>>>>>>>>>>> picture retrieval successful !!")
      postRequest(picType, aPic = pic_ME.copy(name="new name", ownerID=user_ME.userID,pictureID = myPics.head, containingAlbum = profile_ME.defaultAlbum)) //TODO change saving to save not just IDs but entier object

    case PictureUpdated =>
      //log.info("============>>>>>>>>>>>>>>> Picture update successful !!")
      deleteRequest(picType,picId= myPics.head)

    case PictureDeleted =>
      //log.info("============>>>>>>>>>>>>>>> Picture delete successful !!")
*/
      //Test code block ends



  }

}
