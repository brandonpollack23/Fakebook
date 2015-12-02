package system.workers

import java.util.{MissingFormatArgumentException, Date}

import akka.actor
import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import akka.pattern.{pipe, ask}
import akka.util.Timeout
import graphnodes._
import spray.http.{Uri, HttpRequest}
import system.F_BackBone._
import util.MyJsonProtocol

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import spray.json._
import MyJsonProtocol._

import language.postfixOps

//posts can be encrypted or unencrypted (ones on pages are public so unencrypted)
class F_PageProfileHandler(backbone: ActorRef) extends Actor with ActorLogging {
  import F_PageProfileHandler._
  import context.dispatcher

  implicit val timeout = Timeout(5 seconds)

  val pages = collection.mutable.Map[BigInt, F_Page]()
  val profiles = collection.mutable.Map[BigInt, F_UserProfile]()
  val posts = collection.mutable.Map[BigInt, F_PostEOrPost]()

  def receive = {
    case GetProfileInfo(id) =>
      val replyTo = sender()

      profiles.get(id) match {
        case Some(prof) => Future(prof.toJson.compactPrint) pipeTo replyTo
        case None => replyTo ! noSuchProfileFailure(id)
      }

    case GetPageInfo(id) =>
      val replyTo = sender()

      pages.get(id) match {
        case Some(page) => Future(page.toJson.compactPrint) pipeTo replyTo
        case None => replyTo ! noSuchPageFailure(id)
      }

    case GetPostInfo(id) =>
      val replyTo = sender()

      posts.get(id) match {
        case Some(post: F_Post) => Future(post.toJson.compactPrint) pipeTo replyTo
        case Some(post: F_PostE) => Future(post.toJson.compactPrint) pipeTo replyTo
        case None => replyTo ! noSuchPostFailure(id)
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
      deleteUserProfile(id)
  }

  def createUserProfile(userID: BigInt) = {
    val profileID = getUniqueRandomBigInt(profiles)
    val defaultAlbumID = (backbone ? CreateDefaultAlbum(userID)).mapTo[BigInt]
    val defaultProfile = F_UserProfile(List[BigInt](), new Date, List[BigInt](Await.result(defaultAlbumID, 5 seconds)), defaultPictureID, "insert bio here", userID, profileID)
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

      (extractComponent(F_Page.nameString), extractComponent(F_Page.descriptionString), new Date, List[BigInt](), List[BigInt](), List[BigInt](Await.result(defaultAlbumID, 5 seconds)), defaultPictureID, BigInt(extractComponent(F_Page.ownerString), 16), pageID)
    }

    try {
      val newPage = (F_Page.apply _).tupled(getAllComponents)
      pages.put(pageID, newPage)
      val replyTo = sender()
      Future(newPage.toJson.compactPrint) pipeTo replyTo
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
      val replyTo = sender()
      posts.put(postID, newPost)
      Future(newPost.toJson.compactPrint) pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def updatePageData(id: BigInt, request: HttpRequest) = {
    def updateCurrentPageInstance(page: F_Page, params: Uri.Query, parametersRemaining: List[String]): F_Page = {
      if(parametersRemaining.isEmpty) {
        page
      } else {
        val currentParameter = parametersRemaining.head
        params.get(currentParameter) match {
          case Some(value) =>
            currentParameter match {
              case F_Page.`joinPageString` if value == "true" =>
                val newUser = BigInt(params.getOrElse(F_Page.newUserString, throw new MissingFormatArgumentException("missing user id to join page")), 16)
                updateCurrentPageInstance(page.copy(userList = newUser :: page.userList), params, parametersRemaining.tail)
              case F_Page.`leavePageString` if value == "true" =>
                val removeUser = BigInt(params.getOrElse(F_Page.newUserString, throw new MissingFormatArgumentException("missing user id to leave page")), 16)
                updateCurrentPageInstance(page.copy(userList = page.userList.filter(_ != removeUser)), params, parametersRemaining.tail)
              case F_Page.`nameString` => updateCurrentPageInstance(page.copy(name = value), params, parametersRemaining.tail)
              case F_Page.`descriptionString` => updateCurrentPageInstance(page.copy(description = value), params, parametersRemaining.tail)
              case F_Page.`ownerString` => updateCurrentPageInstance(page.copy(ownerID = BigInt(value,16)), params, parametersRemaining.tail)
              case _ => throw new IllegalArgumentException("there is no case to handle such parameter in the list (system issue)")
            }
          case None =>
            updateCurrentPageInstance(page, params, parametersRemaining.tail)
        }
      }
    }

    try{
      val page = pages.getOrElse(id, throw noSuchPageException(List(id)))

      val params = request.uri.query

      val updatedPage = updateCurrentPageInstance(page, params, F_Page.changableParameters)

      pages.put(id, updatedPage)

      val replyTo = sender()

      Future(updatedPage.toJson.compactPrint) pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def updateProfileData(id: BigInt, request: HttpRequest) = {
    def updateCurrentProfileInstance(profile: F_UserProfile, params: Uri.Query, parametersRemaining: List[String]): F_UserProfile = {
      if(parametersRemaining.isEmpty) {
        profile
      } else {
        val currentParameter = parametersRemaining.head
        params.get(currentParameter) match {
          case Some(value) =>
            currentParameter match {
              case F_UserProfile.`profilePictureString` => updateCurrentProfileInstance(profile.copy(profilePictureID = BigInt(value,16)), params, parametersRemaining.tail)
              case F_UserProfile.`descriptionString` => updateCurrentProfileInstance(profile.copy(description = value), params, parametersRemaining.tail)
              case _ => throw new IllegalArgumentException("there is no case to handle such parameter in the list (system issue)")
            }
          case None =>
            updateCurrentProfileInstance(profile, params, parametersRemaining.tail)
        }
      }
    }

    try{
      val profile = profiles.getOrElse(id, throw noSuchProfileException(List(id)))

      val params = request.uri.query

      val updatedProfile = updateCurrentProfileInstance(profile, params, F_Page.changableParameters)

      profiles.put(id, updatedProfile)

      val replyTo = sender()

      Future(updatedProfile.toJson.compactPrint) pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def updatePostData(id: BigInt, request: HttpRequest) = {
    def updateCurrentPostInstance(post: F_Post, params: Uri.Query, parametersRemaining: List[String]): F_Post = {
      if(parametersRemaining.isEmpty) {
        post
      } else {
        val currentParameter = parametersRemaining.head
        params.get(currentParameter) match {
          case Some(value) =>
            currentParameter match {
              case F_Post.`contentsString` => updateCurrentPostInstance(post.copy(contents = value), params, parametersRemaining.tail)
              case _ => throw new IllegalArgumentException("there is no case to handle such parameter in the list (system issue)")
            }
          case None =>
            updateCurrentPostInstance(post, params, parametersRemaining.tail)
        }
      }
    }
    def updateCurrentPostInstanceE(post: F_PostE, params: Uri.Query, parametersRemaining: List[String]): F_PostE = {
      if(parametersRemaining.isEmpty) {
        post
      } else {
        val currentParameter = parametersRemaining.head
        params.get(currentParameter) match {
          case Some(value) =>
            currentParameter match {
              case F_Post.`contentsString` => updateCurrentPostInstanceE(post.copy(contents = value.getBytes), params, parametersRemaining.tail)
              case _ => throw new IllegalArgumentException("there is no case to handle such parameter in the list (system issue)")
            }
          case None =>
            updateCurrentPostInstanceE(post, params, parametersRemaining.tail)
        }
      }
    }

    try{
      val post = posts.getOrElse(id, throw noSuchPostException(List(id)))

      val params = request.uri.query

      val updatedPost = post match {
        case post: F_Post => updateCurrentPostInstance(post, params, F_Page.changableParameters)
        case post: F_PostE => updateCurrentPostInstanceE(post, params, F_Page.changableParameters)
      }

      posts.put(id, updatedPost)

      val replyTo = sender()

      updatedPost match {
        case x: F_Post => Future(x.toJson.compactPrint) pipeTo replyTo
        case x: F_PostE => Future(x.toJson.compactPrint) pipeTo replyTo
      }
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def deletePage(id: BigInt) = {
    pages.remove(id) match {
      case Some(x) =>
        x.albumIDs.foreach(backbone ! DeleteAlbum(_)) //TODO make it so default album is even deleted!!!  best way is probobly special delete default album message
        x.posts.foreach(backbone ! DeletePost(_))
        sender ! "Page Deleted!"
      case None => sender ! noSuchPageFailure(id)
    }
  }

  def deletePost(id: BigInt) = {
    try {
      posts.remove(id) match {
        case Some(x) =>
          if (x.locationType == F_Post.locationPage) {
            val containingPage = pages.getOrElse(x.location, throw noSuchPageException(List(x.location)))
            pages.put(x.location, containingPage.copy(posts = containingPage.posts.filter(_ != x.postID)))
          } else if (x.locationType == F_Post.locationProfile) {
            val containingProfile = profiles.getOrElse(x.location, throw noSuchPageException(List(x.location)))
            profiles.put(x.location, containingProfile.copy(posts = containingProfile.posts.filter(_ != x.postID)))
          }
          sender ! "Post Deleted!"
        case None => sender ! noSuchPostFailure(id)
      }
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def deleteUserProfile(id: BigInt) = {
    profiles.remove(id) match {
      case Some(x) =>
        x.albumIDs.foreach(backbone ! DeleteAlbum(_))
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
