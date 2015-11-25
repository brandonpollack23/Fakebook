package system.workers

import akka.actor
import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import akka.pattern.pipe
import graphnodes.{F_Post, F_UserProfile, F_Page}
import spray.http.HttpRequest
import system.F_BackBone._
import system.jsonFiles.{F_PageJSON, F_ProfileJSON}

import scala.concurrent.Future

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
    ???
  }

  def createPage(request: HttpRequest) = ???

  def createPost(request: HttpRequest) = ???

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
