package system

import java.io.File

import akka.actor.{Terminated, Props, ActorRef, ActorLogging}
import akka.util.Timeout
import spray.can.Http
import spray.http.HttpRequest
import spray.routing.directives.OnCompleteFutureMagnet
import spray.routing._
import spray.http.StatusCodes._

import scala.concurrent.duration._

import akka.pattern.ask

import system.F_BackBone._

import scala.util.{Failure, Success}

import language.postfixOps

class F_Listener(backbone: ActorRef) extends HttpServiceActor with ActorLogging {
  import context.dispatcher

  implicit val timeout = Timeout(5 seconds)

  log.debug("beginning user log\n")

  def receive = {
    case Http.Bound =>
      context.watch(sender)
      context.become {
        runRoute(route) orElse {
          case Terminated(x) =>
            context.stop(self)
        }
      }
  }

  //NOTE: All IDs should be sent in HEX
  //TODO routes broke when i had intermediate lambdas for some reason, but i needed the unmatched path, have to fix it
  val route: Route = { request =>
    pathPrefix("user") {
      log.debug("user path detected")
      get { req =>
        genericGet(req, GetUserInfo)
      } ~
      post { req =>
        pathPrefix("request") {
          genericPost(req, RequestFriend)
        } ~
        genericPost(req, HandleFriendRequest)
      } ~
      delete { req =>
        genericDelete(req, DeleteUser)
      } ~
      path("newuser") {
        put { req =>
          genericPut(CreateUser(req.request))
        }
      }
    } ~
    pathPrefix("data") {
      log.debug("data path detected")
      get { req =>
        getImage(req) //URIs for actual data like pictures
      } ~
      path("uploadimage") {
        put { req =>
          genericPut(PutImage(req.request.entity))
        }
      }
    } ~
    pathPrefix("page") {
      log.debug("page path detected")
      get { req =>
        genericGet(req, GetPageInfo)
      } ~
      post { req =>
        genericPost(req, UpdatePageData)
      } ~
      delete { req =>
        genericDelete(req, DeletePage)
      }
      path("newpage") {
        put { req =>
          genericPut(CreatePage(req.request))
        }
      }
    } ~
    pathPrefix("profile") {
      log.debug("profile path detected")
      get { req =>
        genericGet(req, GetProfileInfo)
      } ~ //no need for "createprofile" they are created with the user and can be accessed through the JSON returned with that creation
      post { req =>
        genericPost(req, UpdateProfileData)
      } //no need to delete profile, deleted with user
    } ~
    pathPrefix("picture") {
      log.debug("picture path detected")
      get { req =>
        genericGet(req, GetPictureInfo)
      } ~ //same as for profile, when you upload an image the picture JSON is created
      post { req =>
        genericPost(req, UpdateImageData)
      } ~
      delete { req =>
        genericDelete(req, DeletePicture)
      }
    } ~
    pathPrefix("album") {
      log.debug("album path detected")
      get { req =>
        genericGet(req, GetAlbumInfo)
      } ~
      post { req =>
        genericPost(req, UpdateAlbumData)
      } ~
      delete { req =>
        genericDelete(req, DeleteAlbum)
      }
      path("createalbum") {
        put { req =>
          genericPut(CreateAlbum(req.request))
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
    else reject
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
    else reject
  }

  /**
   * same as above except no need to parse a special message since the path tells all and this is for putting, so the fully constructed message gets passed here
   * @param message constructed message to send to the backbone for handling
   * @return
   */
  def genericPut(message: PutInfo) = {
    onComplete(OnCompleteFutureMagnet((backbone ? message).mapTo[String])) {
      case Success(newEntityJson) =>
        log.info("put completed successfully: " + message)
        complete(newEntityJson)
      case Failure(ex) =>
        log.error(ex, "put failed: " + message)
        complete(InternalServerError, "Error putting entity: " + ex.getMessage)
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
    else reject
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
        onComplete(OnCompleteFutureMagnet((backbone ? GetImage(idBig)).mapTo[File])) {
          case Success(image) =>
            log.info("get completed successfully: " + GetImage + " " + "for " + idBig)
            getFromFile(image)
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

object F_Listener {
  def props(backbone: ActorRef) = Props(new F_Listener(backbone))
}


/*ideas for speed improvements:
parse out arguments before passing to backbone (might help with scaling to distributed system)
genericDelete, Post, Put, and Get are all pretty similar, some functional composition is probobly possible, esp for delete and get
 */