package clientSim

import java.text.SimpleDateFormat
import java.util.Date
import MatchClasses._
import akka.actor._
import graphnodes._
import scala.util.Random
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
//import spray.json._
//import spray.httpx.SprayJsonSupport
//TODO there is no unique ID in user profile to pass
//TODO way to know a post created is on a profile or a page by its return type


class F_LightUserClient() extends Actor with ActorLogging{

  log.info("=> *Light Client* Logging Started")

  //parameters for User
  var firstName:String = "Ali"
  var lastName:String = "Gator"
  var bio : String = "StudentAtUF"
  var age: Int = 25
  val dobDate :Date= new Date(1989,12,12)
  var description : String = "Gator Profile"
  var description1:String = "new Gator Profile"
  var firstName1:String = "new Ali"
  var lastName1:String= "new Gator"
  var bio1:String = "new Student@UF"

  //parameters for Post
  var postContent: String = "content for a post"
  var locationType1 : String = "profile"
  var locationType2 : String= "page"


  //Page managed by user
  var pageName:String = "Gator Times"
  var pageDes :String = "All about UF"
  var pageName1:String = "new Gator Times"
  var pageDes1:String = "and much more"
  var myPage = F_Page(pageName, pageDes, new Date(1900,1,1), List[BigInt](), List[BigInt](), List[BigInt](), BigInt(0), BigInt(0))


  //default picture signature
  var pName:String = "my Picture"
  var pDes :String = "Vacation"

  var pName1:String = "my new picture"
  var pDes1:String =  "another vacation"

  //album data
  var albmName:String = "my Album"
  var albmDes:String = "spring break"
  var albmName1:String = "my new album"
  var albmDes1:String = "thanksgiving"

  var myProfPosts = List[BigInt]()
  var myPagePosts = List[BigInt]()
  var myPages = List[F_Page]()
  var myPics = List[BigInt]()
  var myAlbums = List[BigInt]()


  var user_ME    =  F_User("", "", "", 0,new Date(1990,1,1), new Date(1990,1,1), List[BigInt](), List[BigInt](), BigInt(0), BigInt(0))
  var profile_ME =  F_UserProfile(List[BigInt](), new Date(1990,1,1), List[BigInt](), BigInt(0), "", BigInt(0))
  val baseRef = context.actorOf(Props(classOf[F_BaseClient]), "baseActor")

  def receive ={

    case Begin =>
      log.info("=> *Begin* match, requesting createUser")
      baseRef ! createUser(firstName, lastName, bio, age, dobDate)

    case userCreated(res) =>
      log.info("=> User creation request complete at end user")
      user_ME = res
      self !  Simulate //getUserProfile(user_ME.userID)

    /*    case userProfileRetrieved(res) =>
          log.info("=> UserProfile retrieved successfully")
          profile_ME = res
          baseRef ! Simulate
    */
    case Simulate =>
      log.info("=> Simulation started")
      val system = ActorSystem("MySystem")
      system.scheduler.schedule(0 milliseconds, 5000 milliseconds )(self ! PerformAction)

    case PerformAction =>
      log.info("=> PerformAction !!")
      val x = Random.nextInt(100)

      if(x<30)  //do some post activity  30 percent
      {
        val z = Random.nextInt(100)
        if (z < 25) {
          baseRef ! createPost(user_ME.userID, postContent, locationType1, profile_ME.profileID)
        }
        if(z>=25 && z<50) {
          if (myPage.ownerID != null)
            baseRef ! createPost(user_ME.userID, postContent, locationType2, myPage.ownerID)
        }
        if (z >= 50 && z < 65) {
          if (profile_ME.posts.nonEmpty)
            baseRef ! getPost(user_ME.userID, profile_ME.posts(Random.nextInt(profile_ME.posts.length))) //from profile
        }
        if(z >= 65 && z<75) {
          if (myPage.posts.nonEmpty)
            baseRef ! getPost(user_ME.userID, myPage.posts(Random.nextInt(myPage.posts.length))) //from page
        }
        if (z >= 75 && z < 85) {
          if (profile_ME.posts.nonEmpty)
            baseRef ! updatePost(user_ME.userID, profile_ME.posts(Random.nextInt(profile_ME.posts.length))) //from profile
        }
        if(z>=85 && z< 90){
          if (myPage.posts.nonEmpty)
            baseRef ! updatePost(user_ME.userID, myPage.posts(Random.nextInt(myPage.posts.length))) //from page
        }
        if(z>=90 && z<= 95) {
          if (profile_ME.posts.nonEmpty)
            baseRef ! deletePost(user_ME.userID, profile_ME.posts(Random.nextInt(profile_ME.posts.length))) //from profile
        }
        else{
          if (myPage.posts.nonEmpty)
            baseRef ! deletePost(user_ME.userID, myPage.posts(Random.nextInt(myPage.posts.length))) //from page
        }
      }



      if(x>=30 && x<60)//do some picture/album activity 30 percent
      {
        val y = Random.nextInt(100)
        if(y<=70) {
          val z = Random.nextInt(100)
          if (z <= 50) {
            val indx = Random.nextInt(profile_ME.albumIDs.length)
            baseRef ! uploadPicture(pName, pDes, profile_ME.albumIDs(indx), user_ME.userID)
          }
          if (z > 50 && z < 75)
            if(myPics.nonEmpty){
              val indx = Random.nextInt(myPics.length)
              baseRef ! getPictureData(user_ME.userID, myPics(indx))
            }
          if (z >= 75 && z < 90)
            if(myPics.nonEmpty) {
              val indx = Random.nextInt(myPics.length)
              baseRef ! updatePictureData(pName1, pDes1, user_ME.userID, myPics(indx))
            }
            else
            if(myPics.nonEmpty) {
              val indx = Random.nextInt(myPics.length)
              baseRef ! deletePicture(user_ME.userID, myPics(indx))
            }
        }
        else {
          val z = Random.nextInt(100)
          if (z <= 50)
            baseRef ! createAlbum(user_ME.userID, albmName, albmDes)
          if (z > 50 && z < 75) {
            val indx = Random.nextInt(profile_ME.albumIDs.length)
            baseRef ! getAlbumData(user_ME.userID, profile_ME.albumIDs(indx))
          }
          if (z >= 75 && z < 90) {
            val indx = Random.nextInt(profile_ME.albumIDs.length)
            baseRef ! updateAlbumData(user_ME.userID, profile_ME.albumIDs(indx), albmName1, albmDes1 )
          }
          else {
            val indx = Random.nextInt(profile_ME.albumIDs.length)
            // if(indx!=0)
            // baseRef ! deleteAlbum(user_ME.userID, profile_ME.albumIDs(indx))  //unable to update profile_ME album list
          }
        }
      }



      if(x>=60 && x< 75)//do some profile update activity  15%
      {
        val z = Random.nextInt(100)
        if (z <= 25)
          baseRef ! updateUserData(user_ME.userID, firstName1, lastName1, bio1)
        if (z > 25 && z< 50)
          baseRef ! getUserData(user_ME.userID)
        if(z>50 && z< 75)
          baseRef ! updateUserProfile(profile_ME.profileID, description1)
        else
          baseRef ! getUserProfile(profile_ME.profileID)
      }


      if(x>=75 && x< 90)//do something about page  15%  //TODO no unique id about page declared in F_Page
      {
        val z = Random.nextInt(100)
        if (z <= 50)
          baseRef ! createPage(user_ME.userID, pageName, pageDes)
        if (z > 50 && z < 75)
          baseRef ! getPageData(user_ME.userID)
        if (z >= 75 && z < 90)
          baseRef ! updatePageData(user_ME.userID, pageName1, pageDes1)
        else
          baseRef ! deletePage(user_ME.userID)

      }
      if(x>=90)//do something about friendList - 10%
      {

      }


    case postCreated(res,locationType) =>
      if(locationType.equalsIgnoreCase("profile")){
        log.info("=> Created profile post received at end user")
        myProfPosts ::= res.postID
      }

      if(locationType.equalsIgnoreCase("page")) {
        log.info("=> Created page post received at end user")
        myPagePosts ::= res.postID
      }

    case postEdited(res) =>
      log.info("=> Edited post received at end user")

    case postRetrieved(res) =>
      log.info("=> Requested post received at end user")

    case postDeleted(res) =>
      log.info("=> postDelete comletion received at end user")


    case userEdited(res) =>
      log.info("=> User Data received at end user")
      user_ME = res

    case userRetrieved(res) =>
      log.info("=> User data received at end user")

    case profileEdited(res) =>
      log.info("=> Profile edit result received at end user")
      profile_ME = res

    case profileRetrieved(res) =>
      log.info("=> Get user Profile complete, response received at end user")


    case pictureUploaded(res) =>
      log.info("=> Picture upload complete, response received at end user")
      myPics ::= res.pictureID

    case pictureEdited(res) =>
      log.info("=> Picture edit complete, response received at end user")


    case pictureRetrieved(res) =>
      log.info("=> Requested picture received at end user")

    case pictureDeleted(res) =>
      log.info("=> Picture Delete request completion received at end user")
      myPics = myPics.filter(_!=res.pictureID)


    case albumCreated(res) =>
      log.info("=> Create album response received at end user")
      myAlbums ::= res.id
      log.info("=> New album added to profile at end user")

    case albumEdited(res) =>
      log.info("=> Album edit successful, response received at end user")

    case albumRetrieved(res) =>
      log.info("=> Retrieve album successful, response received at end user")

    case albumDeleted(res) =>
      log.info("=> Album delete successful, response received at end user")
      //profile_ME.albumIDs = profile_ME.albumIDs.filter(_!=res.id)
      log.info("=> Profile edited with deleted album retracted")

    case pageCreated(res) =>
      log.info("=> Page created, completion received at end user")
      myPages ::= res

    case pageEdited(res) =>
      log.info("=> Page edited, completion received at end user")

    case pageRetrieved(res) =>
      log.info("=> Page retrieved, completion received at end user")

    case pageDeleted(res) =>
      log.info("=> Page deleted, completion received at user end")


  }



}
