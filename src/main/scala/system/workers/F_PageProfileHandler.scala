package system.workers

import java.util.Date

import akka.actor
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import graphnodes._
import spray.http.HttpRequest
import spray.json._
import system.F_BackBone._
import util.MyJsonProtocol._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

//posts can be encrypted or unencrypted (ones on pages are public so unencrypted)
//TODO consider going back to polymorphism for posts
class F_PageProfileHandler(backbone: ActorRef) extends Actor with ActorLogging {
  import F_PageProfileHandler._
  import context.dispatcher

  implicit val timeout = Timeout(5 seconds)

  val pages = collection.mutable.Map[BigInt, F_Page]()
  val profiles = collection.mutable.Map[BigInt, F_UserProfileE]()
  val posts = collection.mutable.Map[BigInt, Either[F_Post, F_PostE]]()

  def receive = {
    case GetProfileInfo(id) => //get will just send the encrypted data no problem, if they can decrypt it they can have it, but only friends will have the key
      val replyTo = sender()

      profiles.get(id) match {
        case Some(prof) => Future(prof.toJson.compactPrint).mapTo[String] pipeTo replyTo
        case None => replyTo ! noSuchProfileFailure(id)
      }

    case GetPageInfo(id) => //pages aren't encrypted due to their public nature, so just give it to whoever wants it
      val replyTo = sender()

      pages.get(id) match {
        case Some(page) => Future(page.toJson.compactPrint).mapTo[String] pipeTo replyTo
        case None => replyTo ! noSuchPageFailure(id)
      }

    case GetPostInfo(id) => //posts may or may not be encrypted, if they ARE encrypted it's because they are on a profile, either way the client will know what to do with it, so just send it
      val replyTo = sender()

      posts.get(id) match {
        case Some(post: Left) => Future(post.toJson.compactPrint).mapTo[String] pipeTo replyTo
        case Some(post: Right) => Future(post.toJson.compactPrint).mapTo[String] pipeTo replyTo
        case None => replyTo ! noSuchPostFailure(id)
      }

    case CreateUserProfile(userID) => //creates an empty user profile for the user
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
      deleteUserProfile(id)
  }

  def createUserProfile(userID: BigInt) = {
    try {
      val profileID = getUniqueRandomBigInt(profiles)
      val defaultAlbumID = Await.result((backbone ? CreateDefaultAlbum(userID)).mapTo[BigInt], 5 seconds)
      val defaultProfile = F_UserProfileE(List[BigInt](), new Date, defaultAlbumID, List[BigInt](), defaultPictureID, Array[Byte](), userID, profileID)
      profiles.put(profileID, defaultProfile)
      profileID
    } catch {
      case ex: Exception =>
        actor.Status.Failure(ex)
    }
  }

  def createPage(request: HttpRequest) = {
    val pageID = getUniqueRandomBigInt(pages)
    val defaultAlbumID = Await.result((backbone ? CreateDefaultAlbum(pageID)).mapTo[BigInt], 5 seconds)

    try {
      val newPage = request.entity.asString.parseJson.convertTo[F_Page].copy(defaultAlbumID = defaultAlbumID, dateOfCreation = new Date)
      pages.put(pageID, newPage)
      val replyTo = sender()
      Future(newPage.toJson.compactPrint).mapTo[String] pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def createPost(request: HttpRequest) = { //could be rewritten much better
    val postID = getUniqueRandomBigInt(posts)

    try {
      var location: BigInt = 0
      val isProfile = request.uri.query.get(F_Post.locationTypeString).get == F_Post.locationProfile
      val newPost = if(isProfile) {
        val pst = request.entity.asString.parseJson.convertTo[F_PostE].copy(dateOfCreation = new Date, postID = postID)
        location = pst.location
        Right(pst)
      } else {
        val pst = request.entity.asString.parseJson.convertTo[F_Post].copy(dateOfCreation = new Date, postID = postID)
        location = pst.location
        Left(pst)
      }

      val replyTo = sender()
      posts.put(postID, newPost)

      newPost match {
        case Left(_) =>
          pages.get(location) match {
            case Some(page) =>
              pages.put(location, page.copy(posts = postID :: page.posts))
            case None =>
              throw new IllegalArgumentException("That page does not exist to post on")
          }
        case Right(_) =>
          profiles.get(location) match {
            case Some(profile) =>
              profiles.put(location, profile.copy(posts = postID :: profile.posts))
            case None =>
              throw new IllegalArgumentException("That profile does not exist to post on")
          }
      }

      Future(newPost.toJson.compactPrint).mapTo[String] pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def updatePageData(id: BigInt, request: HttpRequest) {
    def updatePage(page: F_Page, fields: Map[String, JsValue]): F_Page = {
      if(fields.isEmpty) page
      else {
        val currentField = fields.head
        currentField._1 match {
          case F_Page.`nameField` =>
            updatePage(page.copy(name = currentField._2.toString()), fields.tail)
          case F_Page.`descriptionField` =>
            updatePage(page.copy(description = currentField._2.toString()), fields.tail)
          case _ =>
            updatePage(page, fields.tail)
        }
      }
    }

    try {
      val page = pages.getOrElse(id, throw new NoSuchElementException("That page does not exist"))
      val updatedPage = updatePage(page, request.entity.asString.parseJson.asJsObject.fields)

      pages.put(id, updatedPage)

      val replyTo = sender()

      Future(updatedPage.toJson.compactPrint).mapTo[String] pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def updateProfileData(id: BigInt, request: HttpRequest) = {
    def updateProfile(profile: F_UserProfileE, fields: Map[String, JsValue]): F_UserProfileE = {
      if(fields.isEmpty) profile
      else {
        val currentParameter = fields.head
        currentParameter._1 match {
          case F_UserProfile.`profilePictureIDField` =>
            //TODO check if the album ID exists
            updateProfile(profile.copy(profilePictureID = BigInt(currentParameter._2.toString(), 16)), fields.tail)
          case F_UserProfile.`descriptionField` => updateProfile(profile.copy(description = currentParameter._2.toString().getBytes), fields.tail)
          case _ =>
            updateProfile(profile, fields.tail)
        }
      }
    }

    try{
      val profile = profiles.getOrElse(id, throw noSuchProfileException(List(id)))
      val updateFields = request.entity.asString.parseJson.asJsObject.fields

      val updatedProfile = updateProfile(profile, updateFields)

      profiles.put(id, updatedProfile)

      val replyTo = sender()

      Future(updatedProfile.toJson.compactPrint).mapTo[String] pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def updatePostData(id: BigInt, request: HttpRequest) = { //TODO continue chagjign to entity mode from here
    def updateCurrentPost(post: F_Post, fields: Map[String, JsValue]): F_Post = {
      if(fields.isEmpty) post else {
        val currentField = fields.head
        currentField._1 match {
          case F_Post.`contentsField` => updateCurrentPost(post.copy(contents = currentField._2.toString()), fields.tail)
          case _ => updateCurrentPost(post, fields.tail)
        }
      }
    }
    def updateCurrentPostE(post: F_PostE, fields: Map[String, JsValue]): F_PostE = {
      if(fields.isEmpty) {
        post
      } else {
        val currentField = fields.head
        currentField._1 match {
          case F_Post.`contentsField` => updateCurrentPostE(post.copy(contents = currentField._2.convertTo[Array[Byte]]), fields.tail)
          case _ => updateCurrentPostE(post, fields.tail)
        }
      }
    }

    try{
      val post = posts.getOrElse(id, throw noSuchPostException(List(id)))

      val updateFields = request.entity.asString.parseJson.asJsObject.fields

      val updatedPost = post match {
        case Left(p) => Left(updateCurrentPost(p, updateFields))
        case Right(p) => Right(updateCurrentPostE(p, updateFields))
      }

      posts.put(id, updatedPost)

      val replyTo = sender()

      updatedPost match {
        case x: F_Post => Future(x.toJson.compactPrint).mapTo[String] pipeTo replyTo
        case x: F_PostE => Future(x.toJson.compactPrint).mapTo[String] pipeTo replyTo
      }
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def deletePage(id: BigInt) = {
    pages.remove(id) match {
      case Some(x) =>
        x.albumIDs.foreach(backbone ! DeleteAlbum(_))
        backbone ! DeleteAlbum(x.defaultAlbumID, defaultOverride = true)
        x.posts.foreach(backbone ! DeletePost(_))
        sender ! "Page Deleted!"
      case None => sender ! noSuchPageFailure(id)
    }
  }

  def deletePost(id: BigInt) = {
    try {
      posts.remove(id) match {
        case Some(Left(x)) =>
          val containingPage = pages.getOrElse(x.location, throw noSuchPageException(List(x.location)))
          pages.put(x.location, containingPage.copy(posts = containingPage.posts.filter(_ != x.postID)))
        case Some(Right(x)) =>
          val containingProfile = profiles.getOrElse(x.location, throw noSuchPageException(List(x.location)))
          profiles.put(x.location, containingProfile.copy(posts = containingProfile.posts.filter(_ != x.postID)))
        case None => sender ! noSuchPostFailure(id)
      }

      sender ! "Post Deleted!"
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def deleteUserProfile(id: BigInt) = {
    profiles.remove(id) match {
      case Some(x) =>
        x.albumIDs.foreach(backbone ! DeleteAlbum(_))
        backbone ! DeleteAlbum(x.defaultAlbum)
      case None =>
        log.error("No such profile with id " + id)
    }
  }
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
  private def noSuchProfileException(id: Seq[BigInt]) = new NoSuchElementException("there is no profile entry for " + id.map(x => x.toString(16)).mkString(", "))

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
  private def noSuchPostException(id: Seq[BigInt]) = new NoSuchElementException("there is no post entry for " + id.map(x => x.toString(16)).mkString(", "))

  /**
   * message to send when picture id does not exist
   * @param ids id
   * @return message
   */
  private def noSuchPostFailure(ids: BigInt*) = actor.Status.Failure(noSuchPostException(ids))
}
