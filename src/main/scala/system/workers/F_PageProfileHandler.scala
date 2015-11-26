package system.workers

import java.util.{MissingFormatArgumentException, Date}

import akka.actor
import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import akka.pattern.{pipe, ask}
import graphnodes.{F_Post, F_UserProfile, F_Page}
import spray.http.HttpRequest
import system.F_BackBone
import system.F_BackBone._
import system.jsonFiles.{F_PostJSON, F_PageJSON, F_ProfileJSON}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import language.postfixOps

class F_PageProfileHandler(backbone: ActorRef) extends Actor with ActorLogging {
  import F_PageProfileHandler._
  import context.dispatcher

  val pages = collection.mutable.Map[BigInt, F_Page]()
  val profiles = collection.mutable.Map[BigInt, F_UserProfile]()
  val posts = collection.mutable.Map[BigInt, F_Post]()

  def receive = {
    case GetProfileInfo(id) =>
      val replyTo = sender

      profiles.get(id) match {
        case Some(prof) => Future(F_ProfileJSON.getJSON(prof)) pipeTo replyTo
        case None => replyTo ! noSuchProfileFailure(id)
      }

    case GetPageInfo(id) =>
      val replyTo = sender

      pages.get(id) match {
        case Some(page) => Future(F_PageJSON.getJSON(page)) pipeTo replyTo
        case None => replyTo ! noSuchPageFailure(id)
      }

    case CreateUserProfile(userID) =>
      sender ! createUserProfile(userID) //returns profileID, this is an intersystem message so it is handled different

    case CreatePage(request) =>
      createPage(request)

    case CreatePost(request) =>
      createPost(request)

    case UpdatePageData(id, request) =>
      updatePageData(id, request)

    case UpdateProfileData(id, request) =>
      updateProfileData(id, request)

    case UpdatePostData(id, request) =>
      updatePostData(id, request)

    case DeletePage(id) =>
      deletePage(id)

    case DeletePost(id) =>
      deletePost(id)

    case DeleteUserProfile(id) =>
      sender ! deleteUserProfile(id) //also intersystem
  }

  def createUserProfile(userID: BigInt) = {
    val profileID = getUniqueRandomBigInt(profiles)
    val defaultAlbumID = (backbone ? CreateDefaultAlbum(userID)).mapTo[BigInt]
    val defaultProfile = F_UserProfile(List[BigInt](), new Date, List[BigInt](Await.result(defaultAlbumID, 5 seconds)), defaultPictureID, "insert bio here", profileID)
    profiles.put(profileID, defaultProfile)
    profileID
  }

  def createPage(request: HttpRequest) = {
    val pageID = getUniqueRandomBigInt(pages)
    val params = request.uri.query
    val defaultAlbumID = (backbone ? CreateDefaultAlbum(pageID)).mapTo[BigInt]

    def getAllComponents = {
      def extractComponent(key: String) = {
        params.find(_._1 == key).map(_._2) match {
          case Some(x) => x
          case None => throw new MissingFormatArgumentException("there is no entry for " + key)
        }
      }

      (extractComponent(F_Page.nameString), extractComponent(F_Page.descriptionString), new Date, List[BigInt](), List[BigInt](), List[BigInt](Await.result(defaultAlbumID, 5 seconds)), defaultPictureID, BigInt(extractComponent(F_Page.ownerString), 16))
    }

    try {
      val newPage = (F_Page.apply _).tupled(getAllComponents)
      pages.put(pageID, newPage)
      val replyTo = sender
      Future(F_PageJSON.getJSON(newPage)) pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def createPost(request: HttpRequest) = {
    val postID = getUniqueRandomBigInt(posts)
    val params = request.uri.query

    def getAllComponents = {
      def extractComponent(key: String) = {
        params.find(_._1 == key).map(_._2) match {
          case Some(x) => x
          case None => throw new MissingFormatArgumentException("there is no entry for " + key)
        }
      }

      (extractComponent(F_Post.contentsString), BigInt(extractComponent(F_Post.creatorString), 16), extractComponent(F_Post.locationTypeString), BigInt(extractComponent(F_Post.locationString), 16), new Date, postID)
    }

    try {
      val newPost = (F_Post.apply _).tupled(getAllComponents)
      val replyTo = sender
      posts.put(postID, newPost)
      Future(F_PostJSON.getJSON(newPost)) pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def updatePageData(id: BigInt, request: HttpRequest) = ???

  def updateProfileData(id: BigInt, request: HttpRequest) = ???

  def updatePostData(id: BigInt, request: Any) = ???

  def deletePage(id: BigInt) = ???

  def deletePost(id: BigInt) = ???

  def deleteUserProfile(id: BigInt) = ???
}

object F_PageProfileHandler {
  def props(backbone: ActorRef) = Props(new F_PageProfileHandler(backbone))

  /**
   * exception to throw or put in messages when no such picture
   * @param id id
   * @return exception
   */
  private def noSuchPageException(id: Seq[BigInt]) = new NoSuchElementException("there is no page entry for " + id.mkString(", "))

  /**
   * message to send when picture id does not exist
   * @param ids id
   * @return message
   */
  private def noSuchPageFailure(ids: BigInt*) = actor.Status.Failure(noSuchPageException(ids))

  /**
   * exception to throw or put in messages when no such picture
   * @param id id
   * @return exception
   */
  private def noSuchProfileException(id: Seq[BigInt]) = new NoSuchElementException("there is no profile entry for " + id.mkString(", "))

  /**
   * message to send when picture id does not exist
   * @param ids id
   * @return message
   */
  private def noSuchProfileFailure(ids: BigInt*) = actor.Status.Failure(noSuchProfileException(ids))

  /**
   * exception to throw or put in messages when no such picture
   * @param id id
   * @return exception
   */
  private def noSuchPostException(id: Seq[BigInt]) = new NoSuchElementException("there is no post entry for " + id.mkString(", "))

  /**
   * message to send when picture id does not exist
   * @param ids id
   * @return message
   */
  private def noSuchPostFailure(ids: BigInt*) = actor.Status.Failure(noSuchPostException(ids))
}
