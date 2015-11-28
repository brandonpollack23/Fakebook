package system

import akka.actor._
import akka.event.{Logging, LoggingAdapter}
import akka.util.Timeout
import spray.can.Http
import spray.http.{HttpResponse, HttpRequest}
import spray.routing.directives.{DebuggingDirectives, OnCompleteFutureMagnet}
import spray.routing._
import spray.http.StatusCodes._

import scala.concurrent.Await
import scala.concurrent.duration._

import akka.pattern.ask

import system.F_BackBone._

import scala.util.{Failure, Success}

import language.postfixOps

class F_Listener(backBoneActor: ActorRef) extends Actor with F_ListenerService with ActorLogging {
  override def backbone: ActorRef = backBoneActor

  def actorRefFactory = context

  log.debug("beginning user log\n")

  def receive = {
    case Http.Bound =>
      context.watch(sender)
      context.become {
        runRoute(route) orElse {
          case Terminated(_) =>
            context.stop(self)
        }
      }
  }
}

object F_Listener {
  def props(backbone: ActorRef) = Props(new F_Listener(backbone))
}

trait F_ListenerService extends HttpService {
  def log: LoggingAdapter
  def backbone: ActorRef

  implicit def executionContext = actorRefFactory.dispatcher

  implicit val timeout = Timeout(5 seconds)

  DebuggingDirectives.logRequestResponse("user-get", Logging.DebugLevel)

  //NOTE: All IDs should be sent in HEX
  //TODO get routes working, now that PUT is all inside a complete we should be closer, perhaps it is right but we dont know yet
  val route: Route = { request =>
    pathPrefix("users") { req =>
      log.debug("user path!")
      path("newuser") { req2 =>
        put {
          detach() {
            complete(genericPut(CreateUser(req2.request)))
          }
        }
      } ~
      get {
        genericGet(req, GetUserInfo)
      } ~
      post {
        pathPrefix("request") { req2 =>
          genericPost(req2, RequestFriend)
        } ~
          pathPrefix("remove") { req2 =>
            genericPost(req2, RemoveFriend)
          }
        genericPost(req, HandleFriendRequest)
      } ~
      delete {
        genericDelete(req, DeleteUser)
      }
    } ~
    pathPrefix("data") { req =>
      get {
        getImage(req) //URIs for actual data like pictures
      } ~
      path("uploadimage") { req2 =>
        put {
          complete(genericPut(PutImage(req2.request)))
        }
      }
    } ~
    pathPrefix("page") { req =>
      get {
        genericGet(req, GetPageInfo)
      } ~
      post {
        genericPost(req, UpdatePageData)
      } ~
      delete {
        genericDelete(req, DeletePage)
      }
      path("newpage") { req =>
        put {
          complete(genericPut(CreatePage(req.request)))
        }
      }
    } ~
    pathPrefix("profile") { req =>
      get {
        genericGet(req, GetProfileInfo)
      } ~ //no need for "createprofile" they are created with the user and can be accessed through the JSON returned with that creation
      post {
        genericPost(req, UpdateProfileData)
      } //no need to delete profile, deleted with user
    } ~
    pathPrefix("picture") { req =>
      get {
        genericGet(req, GetPictureInfo)
      } ~ //same as for profile, when you upload an image the picture JSON is created
      post {
        genericPost(req, UpdateImageData)
      } ~
      delete {
        genericDelete(req, DeletePicture)
      }
    } ~
    pathPrefix("post") { req =>
      get {
        genericGet(req, GetPostInfo)
      } ~
      post {
        genericPost(req, UpdatePostData)
      } ~
      put {
        complete(genericPut(CreatePost(req.request)))
      } ~
      delete {
        genericDelete(req, DeletePost)
      }
    } ~
    pathPrefix("album") { req =>
      get {
        genericGet(req, GetAlbumInfo)
      } ~
      post {
        genericPost(req, UpdateAlbumData)
      } ~
      delete {
        genericDelete(req, DeleteAlbum)
      }
      path("createalbum") { req2 =>
        put {
          complete(genericPut(CreateAlbum(req2.request)))
        }
      }
    }
  }

  /**
   * goes inside of a spray routing "get" and completes the passed message to the backbone given the id
   * it internally converts the remaining request path to a bigint, if this fails it completes with a failure
   * @param req the reques who contains the string to be converted to bigint as an ID
   * @param messageConstructor the case class message (constructor but can be passed with sugar) to compose the bigint into
   * @return returns a route for thed spray routing to go through and side effects the complete needed
   */
  def genericGet(req: RequestContext, messageConstructor: (BigInt) => GetInfo): Route = {
    val id = req.unmatchedPath.toString

    if (!id.contains("/")) { //if this is the last element only
      try {
        val idBig = BigInt(id, 16)
        onComplete(OnCompleteFutureMagnet((backbone ? messageConstructor.apply(idBig)).mapTo[String])) {
          case Success(entityJson) =>
            log.info("get completed successfully: " + messageConstructor + " " + "for " + idBig)
            complete(entityJson)
          case Failure(ex) =>
            log.error(ex, "get failed: " + messageConstructor + " for " + idBig)
            complete(InternalServerError, "Request could not be completed: \n" + ex.getMessage)
        }
      } catch {
        case e: NumberFormatException =>
          log.info("illegally formatted id requested: " + id)
          complete(BadRequest, "Numbers are formatted incorrectly")
      }
    }
    else {
      log.debug("uri not formatted correctly for a get")
      reject
    }
  }

  /**
   * same as above but for posts, I treid to write a more generic function to repeat rewriting code but it ended up just not being worth the thought
   * @param req request who contains id to parse to bigint and the parameters
   * @param messageConstructor the message to send
   * @return
   */
  def genericPost(req: RequestContext, messageConstructor: (BigInt, HttpRequest) => PostInfo) = {
    val id = req.unmatchedPath.toString

    if (!id.contains("/")) {
      try {
        val idBig = BigInt(id, 16)
        onComplete(OnCompleteFutureMagnet((backbone ? messageConstructor.apply(idBig, req.request)).mapTo[String])) {
          case Success(newEntityJson) =>
            log.info("post completed successfully: " + messageConstructor + " for " + idBig)
            complete(newEntityJson)
          case Failure(ex) =>
            log.error(ex, "get failed: " + messageConstructor + " for " + idBig)
            complete(InternalServerError, "Request could not be completed: \n" + ex.getMessage)
        }
      } catch {
        case e: NumberFormatException =>
          log.info("illegally formatted id requested: " + id)
          complete(BadRequest, "Numbers are formatted incorrectly")
      }
    }
    else {
      log.debug("uri not formatted correctly for a post")
      reject
    }
  }

  /**
   * same as above except no need to parse a special message since the path tells all and this is for putting, so the fully constructed message gets passed here
   * @param message constructed message to send to the backbone for handling
   * @return
   */
  def genericPut(message: PutInfo) = {
    Await.ready((backbone ? message).mapTo[String], 5 seconds).value.get match {
      case Success(newEntityJson) =>
        log.info("put completed successfully: " + message)
        HttpResponse(OK, newEntityJson)
      case Failure(ex) =>
        log.error(ex, "put failed: " + message)
        HttpResponse(InternalServerError, "Error putting entity: " + ex.getMessage)
    }
  }

  /**
   * almost identical to get, should probobly only be one function
   * @param req identical
   * @param messageConstructor identical
   * @return route
   */
  def genericDelete(req: RequestContext, messageConstructor: (BigInt) => DeleteInfo) = {
    val id = req.unmatchedPath.toString

    if (!id.contains("/")) {
      try {
        val idBig = BigInt(id, 16)
        onComplete(OnCompleteFutureMagnet((backbone ? messageConstructor.apply(idBig)).mapTo[String])) {
          case Success(newEntityJson) =>
            log.info("delete completed successfully: " + messageConstructor + " for " + idBig)
            complete(newEntityJson)
          case Failure(ex) =>
            log.error(ex, "get failed: " + messageConstructor + " for " + idBig)
            complete(InternalServerError, "Request could not be completed: \n" + ex.getMessage)
        }
      } catch {
        case e: NumberFormatException =>
          log.info("illegally formatted id requested: " + id)
          complete(BadRequest, "Numbers are formatted incorrectly")
      }
    }
    else {
      log.debug("uri not formatted correctly for delete")
      reject
    }
  }

  /**
   * Gets image and streams it back from file
   * @param req request context
   * @return something?
   */
  def getImage(req: RequestContext) = {
    val id = req.unmatchedPath.toString

    if (!id.contains("/")) { //if this is the last element only
      try {
        val idBig = BigInt(id, 16)
        onComplete(OnCompleteFutureMagnet((backbone ? GetImage(idBig)).mapTo[Array[Byte]])) {
          case Success(image) =>
            log.info("get completed successfully: " + GetImage + " " + "for " + idBig)
            complete(image)
          case Failure(ex) =>
            log.error(ex, "get failed: " + GetImage + " for " + idBig)
            complete(InternalServerError, "Request could not be completed: \n" + ex.getMessage)
        }
      } catch {
        case e: NumberFormatException =>
          log.info("illegally formatted id requested: " + id)
          complete(BadRequest, "Numbers are formatted incorrectly")
      }
    }
    else reject
  }
}

/*ideas for speed improvements:
parse out arguments before passing to backbone (might help with scaling to distributed system)
genericDelete, Post, Put, and Get are all pretty similar, some functional composition is probobly possible, esp for delete and get
 */