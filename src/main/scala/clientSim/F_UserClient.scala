package clientSim

import java.security.KeyPair
import javax.crypto.KeyGenerator
import util.MyJsonProtocol._
import util.Crypto
import akka.actor.Actor
import spray.client.pipelining._
import spray.client.pipelining.sendReceive
import scala.util.{Success, Failure}
import java.text.SimpleDateFormat._
import spray.httpx.SprayJsonSupport._
import spray.json.DefaultJsonProtocol
import spray.httpx.unmarshalling._
import spray.httpx.marshalling._
import spray.http._
import spray.json._
import java.util.Date
import akka.actor._
import graphnodes._
import CaseClasses._
import scala.util.Random
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import javax.crypto.SecretKey
import java.security.KeyPairGenerator
import spray.io.ClientSSLEngineProvider




class F_UserClient extends Actor with ActorLogging {

  implicit val myEngineProvider = ClientSSLEngineProvider { engine =>
    engine.setEnabledCipherSuites(Array("TLS_RSA_WITH_AES_256_CBC_SHA"))
    engine.setEnabledProtocols(Array("SSLv3", "TLSv1"))
    engine
  }

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
  val removeFriend  : String = "removeFriend"

  //AES Encryption
  val kGen: KeyGenerator = KeyGenerator.getInstance("AES")
  kGen.init(128)
  val aesKey: SecretKey = kGen.generateKey()

  //RSA Encryption
  val kpg: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
  kpg.initialize(2048)
  val kp: KeyPair = kpg.genKeyPair()
  val publicKey  = kp.getPublic()
  val privateKey = kp.getPrivate()


  //default profiles for Put requests
  var user_ME    = F_User("Ali", "Gator", "Student at UF", 25, new Date(1989-1900,1,1), new Date(2015-1900,1,1), List[(BigInt, SecretKey)](), List[(BigInt, Array[Byte])](), 0, publicKey, 0)
  var profile_ME = F_UserProfile(List[BigInt](), new Date(2015-1900,1,1), 0, List[BigInt](), 0, "My Profile", 0, 0)
  var page_ME    = F_Page("Gator Times", "All about UF", new Date(2015-1900,1,1), List[BigInt](), List[BigInt](), 0, List[BigInt](), 0, 0, 0)
  var post_ME   = F_Post("Some News", 0, "profile", 0, new Date(2015-1900,1,1), 0)
  var album_ME :F_Album   = F_Album("My Album", "Vacations", new Date(2015-1900,1,1), false, 0, 0, List[BigInt]())
  var pic_ME :F_Picture    = F_Picture("My Pic", "Holidays", 0, new Date(2015-1900,1,1), 0, 0, 0)


  var userId    : BigInt = 0
  var profileId : BigInt = 0

  //Lists to record current status
  var myPosts  = List[BigInt]()
  var myPagePosts = List[BigInt]()
  var myProfPosts = List[BigInt]()
  var myPics   = List[BigInt]()
  var myAlbums = List[BigInt]()
  var myPages  = List[BigInt]()

  //TODO check about the id required for get and delete, and cookie authentication code to be implemented
  //TODO friend request code

  def initialize() ={
    //send create user request, get user profile, set values in all the objects made for further requests

    //create user
    //get profile
    //set userId and profileID values in all objects
    //create page
    //create album
    //set values in post objects


  }

  def authRequest() = {

    val uri = Uri("https://localhost:8080/users/auth/"+userId.toString(16))

    val pipeline = sendReceive ~> unmarshal[String]
    val responseFuture = pipeline {Post(uri)}

    responseFuture onComplete {

      case Success(jsonRef) =>
       //TODO add cookie code here


      case Failure(error) =>
        log.error(error, "Failed to fetch authorization cookie " + error.getMessage)

    }

  }



  def putRequest(reqType:String, aUser:F_User, aPage:F_Page=null, aPost:F_Post=null, aAlbum:F_Album=null, aPic:F_Picture=null) = reqType match{


    case "user" =>

      val uri = Uri("https://localhost:8080/users/newuser")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, aUser.encryptUser(aesKey).toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          user_ME = jsonRef.parseJson.convertTo[F_UserE].decryptUserE(aesKey,privateKey)
          userId = user_ME.userID
          profileId = user_ME.profileID
          log.info("==============>>>>>>>> user profile created")
          self ! UserCreated()

        case Failure(error) =>
          log.error(error, "Failed to run Create User because of " + error.getMessage)
      }


    case "page" =>

      val uri = Uri("http://localhost:8080/page/newpage")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, aPage.toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("==============>>>>>>>> Page creation successful!!")
          myPages ::= jsonRef.parseJson.convertTo[F_Page].ID
          //self ! Simulate()//#
          self ! PageCreated()

        case Failure(error) =>
          log.error(error, "Couldn't get Page Created !!")
        //self ! Simulate()//#
      }


    case "post" =>

      val uri = Uri("http://localhost:8080/post")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, aPost.encryptPost(aesKey).toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("==============>>>>>>>> createPost successful!!")
          val temp:F_Post = jsonRef.parseJson.convertTo[F_PostE].decryptPost(aesKey)
          myPosts ::= temp.postID
          if(temp.locationType=="profile")
            myProfPosts ::= temp.postID
          else{
            myPagePosts ::= temp.postID
          }
          //self ! Simulate()//#
          self ! PostCreated()

        case Failure(error) =>
          log.error(error, "Couldn't run createPost  !!")
        //self ! Simulate()//#
      }


    case "album" =>

      val uri = Uri("http://localhost:8080/album/createalbum")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, aAlbum.encryptAlbum(aesKey).toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("============>>>>>>>>>>> Album successfully Created !!")
          myAlbums ::= jsonRef.parseJson.convertTo[F_AlbumE].decryptAlbumE(aesKey).id
          //self ! Simulate()//#
          self ! AlbumCreated()

        case Failure(error) =>
          log.error(error, "Couldn't run createAlbum  !!")
        //self ! Simulate()//#
      }


    case "picture" =>

      val uri = Uri("http://localhost:8080/data/uploadimage")

      val pipeline = sendReceive ~> unmarshal[String]
      val picTrans:F_PictureTransmit = F_PictureTransmit(aPic.encryptPicture(aesKey), Array[Byte](1,2,3))
      val responseFuture = pipeline {Put(uri, HttpEntity(MediaTypes.`application/json`, picTrans.toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("uploadPicture successful!!")
          myPics ::= jsonRef.parseJson.convertTo[F_PictureE].decryptPictureE(aesKey).pictureID
          //self ! Simulate()//#
          self ! PictureUploaded()

        case Failure(error) =>
          log.error(error, "Couldn't run uploadPicture !!")
        //self ! Simulate()//#
      }

  }


  def getRequest(reqType:String, pageId:BigInt=0, postId:BigInt=0, albumId:BigInt=0, picId:BigInt=0) = reqType match {

    case "user" =>

      val uri = Uri("http://localhost:8080/users/"+userId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}

      responseFuture onComplete {
        case Success(jsonRef) =>
          log.info("==============>>>>>>>> User Data successfully Retrieved!!")
          //self ! Simulate()//#
          self ! UserDataRetrieved()

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve User Data !!")
        //self ! Simulate()//#
      }


    case "profile" =>

      val uri = Uri("http://localhost:8080/profile/" + profileId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          profile_ME = jsonRef.parseJson.convertTo[F_UserProfileE].decryptUserProfileE(aesKey)
          log.info("==============>>>>>>>> user Profile Retrieved successfully")
          self ! ProfileRetrieved()

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve User Profile !!")
        //self ! Simulate()//#
      }


    case "page" =>

      val uri = Uri("http://localhost:8080/page/" + pageId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("Page Data successfully Retrieved !!")
          //self ! Simulate()//#
          self ! PageRetrieved()

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve Page Data !!")
        //self ! Simulate()//#
      }


    case "post" =>

      val uri = Uri("http://localhost:8080/post/"+postId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("==============>>>>>>>> Post successfully Retrieved !!")
          //self ! Simulate()//#
          self ! PostRetrieved()

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve Post !!")
        //self ! Simulate()//#
      }


    case "album" =>

      val uri = Uri("http://localhost:8080/album/"+albumId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("Album data successfully Retrieved !!")
          //self ! Simulate()//#
          self ! AlbumRetrieved()

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve Album Data !!")
        //self ! Simulate()//#
      }


    case "picture" =>

      val uri = Uri("http://localhost:8080/picture/"+picId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Get(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("getPictureData successful!!")
          //self ! Simulate()//#
          self ! PictureRetrieved()

        case Failure(error) =>
          log.error(error, "Couldn't Retrieve Picture Data !!")
        //self ! Simulate()//#
      }



  }


  def postRequest(reqType:String, aUser:F_User, aProfile:F_UserProfile=null, aPage:F_Page=null, aPost:F_Post=null, aAlbum:F_Album=null, aPic:F_Picture=null) = reqType match {

    case "user" =>

      val uri = Uri("http://localhost:8080/users/")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aUser.encryptUser(aesKey).toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("updateUserData successful!!")
          user_ME = jsonRef.parseJson.convertTo[F_UserE].decryptUserE(aesKey, privateKey)
          //self ! Simulate()//#
          self ! UserUpdated()

        case Failure(error) =>
          log.error(error, "Couldn't run updateUserData  !!")
        //self ! Simulate()//#
      }


    case "profile" =>

      val uri = Uri("http://localhost:8080/profile/")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aProfile.encryptUserProfile(aesKey).toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("updateUserData successful!!")
          profile_ME = jsonRef.parseJson.convertTo[F_UserProfileE].decryptUserProfileE(aesKey)
          //self ! Simulate()//#
          self ! ProfileUpdated()

        case Failure(error) =>
          log.error(error, "Couldn't run updateUserProfile !!")
        //self ! Simulate()//#
      }


    case "page" =>

      val uri = Uri("http://localhost:8080/page/")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aPage.toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("updatePageData successful !!")
          //self ! Simulate()//#
          self ! PageUpdated()

        case Failure(error) =>
          log.error(error, "Couldn't run updatePageData !!")
        //self ! Simulate()//#
      }


    case "post" =>

      val uri = Uri("http://localhost:8080/post/")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aPost.encryptPost(aesKey).toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("updatePost successful!!")
          //self ! Simulate()//#
          self ! PostUpdated()

        case Failure(error) =>
          log.error(error, "Couldn't run updatePost  !!")
        //self ! Simulate()//#
      }


    case "album" =>

      val uri = Uri("http://localhost:8080/album/")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aAlbum.encryptAlbum(aesKey).toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("updateAlbumData successful!!")
          //self ! Simulate()//#
          self ! AlbumUpdated()

        case Failure(error) =>
          log.error(error, "Couldn't run updateAlbumData :(")
        //self ! Simulate()//#
      }


    case "picture" =>

      val uri = Uri("http://localhost:8080/picture/")

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Post(uri, HttpEntity(MediaTypes.`application/json`, aPic.encryptPicture(aesKey).toJson.compactPrint))}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("updatePictureData successful!!")
          //self ! Simulate()//#
          self ! PictureUpdated()

        case Failure(error) =>
          log.error(error, "Couldn't run updatePictureData :(")
        //self ! Simulate()//#
      }


  }


  def deleteRequest(reqType:String, pageId:BigInt=0, postId:BigInt=0, albumId:BigInt=0, picId:BigInt=0) = reqType match {

    case "user" =>
      val uri = Uri("http://localhost:8080/users/"+userId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Delete(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("User deleted successfully !!")
          self ! UserDeleted()

        case Failure(error) =>
          log.error(error, "Couldn't delete User !!")

      }

    case "page" =>

      val uri = Uri("http://localhost:8080/page/"+pageId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Delete(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info(" Delete Page successful !!")
          myPages = myPages.filter(_!=pageId)
          //self ! Simulate()//#
          self ! PageDeleted()

        case Failure(error) =>
          log.error(error, "Couldn't run Delete Page !!")
        //self ! Simulate()//#
      }

    case "post" =>

      val uri = Uri("http://localhost:8080/post/"+postId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Delete(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("Delete Post successful !!")
          myPosts = myPosts.filter(_!=postId)
          myPagePosts = myPagePosts.filter(_!=postId)
          myProfPosts = myProfPosts.filter(_!=postId)
          //self ! Simulate()//#
          self ! PostDeleted()

        case Failure(error) =>
          log.error(error, "Couldn't run deletePost :(")
          self ! Simulate()//#
      }

    case "album" =>

      val uri = Uri("http://localhost:8080/album/"+albumId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Delete(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("deleteAlbum successful!!")
          myAlbums = myAlbums.filter(_!=albumId)
          //self ! Simulate()//#
          self ! AlbumDeleted()

        case Failure(error) =>
          log.error(error, "Couldn't run deleteAlbum :(")
        //self ! Simulate()//#
      }

    case "picture" =>

      val uri = Uri("http://localhost:8080/picture/"+picId.toString(16))

      val pipeline = sendReceive ~> unmarshal[String]
      val responseFuture = pipeline {Delete(uri)}

      responseFuture onComplete {

        case Success(jsonRef) =>
          log.info("deletePicture successful!!")
          myPics = myPics.filter(_!=picId)
          //self ! Simulate()//#
          self ! PictureDeleted()

        case Failure(error) =>
          log.error(error, "Couldn't run deletePicture :(")
        //self ! Simulate()//#
      }


  }

  def receive ={

    //Test code block begins
    case Begin =>
      putRequest(userType,user_ME)

    case UserCreated() =>
      log.info("============>>>>>>>>>>>>>>>User creation successful ")

    //Test code block ends

    //original Simulation block begin here
/*
        case Begin =>
          putRequest(userType, user_ME)

        case UserCreated() =>
          getRequest(profileType)

        case ProfileRetrieved() =>
          putRequest(pageType, null, page_ME)

        case PageCreated() =>
          self ! Simulate()

        case Simulate() =>

          val x = Random.nextInt(100)

          //do some Post related activity  30% of Time
          if(x<30)
          {
            val z = Random.nextInt(100)
            if (z < 25) {
              putRequest(postType, null, null, post_ME.copy(creator=userId, location= profileId))
            }
            if(z>=25 && z<50) {
              if (myPages.nonEmpty)
                putRequest(postType, null, null, post_ME.copy(creator=userId, locationType="page", location= myPages.head))
            }
            if (z >= 50 && z < 75) {
              if (myPosts.nonEmpty)
                getRequest(postType, null, myPosts.head)
            }
            if (z >= 75 && z < 90) {
              if (myPagePosts.nonEmpty)
                postRequest(postType, null,null,null, post_ME.copy(contents="new contents",creator=userId, locationType="page",location=myPagePosts.head))
            }
            if(z>=90) {
              if (myPosts.length>1)
                deleteRequest(postType,null, myPosts.head)
            }
          }



          //do some picture/album activity 30 percent
          if(x>=30 && x<60) //Picture
          {
            val y = Random.nextInt(100)
            if(y<=70) {
              val z = Random.nextInt(100)
              if (z <= 50) {
                if(myAlbums.nonEmpty)
                putRequest(picType,null,null,null,null, pic_ME.copy(containingAlbum=myAlbums.head, ownerID = userId))
              }
              if (z > 50 && z < 75)
                if(myPics.nonEmpty){
                  getRequest(picType,0,0,0,myPics.head)
                }
              if (z >= 75 && z < 90)
                if(myPics.nonEmpty) {
                  if(myAlbums.nonEmpty)
                  postRequest(picType,null,null,null,null,null,pic_ME.copy(name="new name", containingAlbum=myAlbums.head, ownerID=userId))
                }
                else
                if(myPics.length>1) {
                  deleteRequest(picType,0,0,0,myPics.head)
                }
            }
            else {    //Album
              val z = Random.nextInt(100)
              if (z <= 50) {
                putRequest(albumType,null,null,null,album_ME.copy(ownerID=userId))
              }
              if (z > 50 && z < 75) {
                if(myAlbums.nonEmpty)
                getRequest(albumType,0,0,myAlbums.head)
              }
              if (z >= 75 && z < 90) {
                if(myAlbums.nonEmpty)
                postRequest(albumType,null,null,null,null,album_ME.copy(name="new album", ownerID=userId,id =myAlbums.head))
              }
              else {
                if(myAlbums.length>1)
                  deleteRequest(albumType,0,0,myAlbums.head)
              }
            }
          }


          //user data and profile data activity  15% of Time
          if(x>=60 && x< 75)
          {
            val z = Random.nextInt(100)
            if (z <= 25)
              postRequest(userType, user_ME.copy(firstName="new name",age=26))
            if (z > 25 && z< 50)
              getRequest(userType)
            if(z>50 && z< 75)
              postRequest(profileType,null,profile_ME.copy(description="new job"))
            else
              getRequest(profileType)
          }


          if(x>=75 && x< 90)//do something about page  15%
          {
            val z = Random.nextInt(100)
            if (z <= 50)
              putRequest(pageType,null,page_ME.copy(ownerID=userId))
            if (z > 50 && z < 75)
              if(myPages.nonEmpty)
                getRequest(pageType,myPages.head)
            if (z >= 75 && z < 90)
              if(myPages.nonEmpty)
                postRequest(pageType,null,null,page_ME.copy(name="new page",ownerID=userId,ID=myPages.head))
              else
              if(myPages.length>1)
                deleteRequest(pageType,myPages.head)

          }
          if(x>=90)//do something about friendList - 10%
          {
            self ! Simulate()
            //postRequest(friendRequest)
            //postRequest(removeFriend)
          }

*/


  }

}
