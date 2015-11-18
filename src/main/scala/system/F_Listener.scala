package system

import akka.actor.{ActorRef, ActorLogging}
import akka.util.Timeout
import spray.http.HttpHeader
import spray.routing.directives.OnCompleteFutureMagnet
import spray.routing.{HttpServiceActor, Route}
import spray.http.StatusCodes._

import scala.concurrent.duration._

import akka.pattern.ask

import system.F_BackBone._

import scala.util.{Failure, Success}

import language.postfixOps

class F_Listener(backbone: ActorRef) extends HttpServiceActor with ActorLogging {
  implicit val timeout = Timeout(5 seconds)

  //TODO must watch the spray side http server actor and die with it

  val route: Route = request =>
    log.info("received request " + request)

    path("data") { req =>
      get { //URIs for actual data like pictures
        genericGet(req.unmatchedPath.toString, GetImage)
      } ~
      path("uploadimage") {
        put { req =>
          genericPut(PutImage(req.request.entity))
        }
      }
    } ~
    path("user") { req =>
      get {
        genericGet(req.unmatchedPath.toString, GetUserInfo)
      } ~
      post {
        genericPost(req.unmatchedPath.toString, req.request.headers, UpdateUserData)
      } ~
      path("newuser") {
        put { req =>
            genericPut(CreateUser(req.request))
        }
      }
    } ~
    path("page") { req =>
      get {
        genericGet(req.unmatchedPath.toString, GetPageInfo)
      } ~
      post {
        genericPost(req.unmatchedPath.toString, req.request.headers, UpdatePageData)
      }
      path("newpage") {
        put { req =>
          genericPut(CreatePage(req.request))
        }
      }
    } ~
    path("profile") { req =>
      get {
        genericGet(req.unmatchedPath.toString, GetProfileInfo)
      } ~ //no need for "createprofile" they are created with the user and can be accessed through the JSON returned with that creation
      post {
        genericPost(req.unmatchedPath.toString, req.request.headers, UpdateProfileData)
      }
    } ~
    path("picture") { req =>
      get {
        genericGet(req.unmatchedPath.toString, GetPictureInfo)
      } ~ //same as for profile, when you upload an image the picture JSON is created
      post {
        genericPost(req.unmatchedPath.toString, req.request.headers, UpdateImageData)
      }
    } ~
    path("album") { req =>
      get {
        genericGet(req.unmatchedPath.toString, GetAlbumInfo)
      } ~
      post {
        genericPost(req.unmatchedPath.toString, req.request.headers, UpdateAlbumData)
      }
      path("createalbum") {
        put { req =>
          genericPut(CreateAlbum(req.request))
        }
      }
    }

  /**
   * goes inside of a spray routing "get" and completes the passed message to the backbone given the id
   * it internally converts the remaining request path to a bigint, if this fails it completes with a failure
   * @param id the string to be converted to bigint as an ID
   * @param messageConstructor the case class message (constructor but can be passed with sugar) to compose the bigint into
   * @return returns a route for thed spray routing to go through and side effects the complete needed
   */
  def genericGet(id: String, messageConstructor: (BigInt) => GetInfo): Route = {
    try {
      val idBig = BigInt(id)
      pathEnd {
        onComplete(OnCompleteFutureMagnet(backbone ? messageConstructor.apply(idBig))) {
          case Success(entityJson) =>
            log.info("get completed successfully: " + messageConstructor + " " + "for " + idBig)
            complete(entityJson)
          case Failure(ex) =>
            log.error(ex, "get failed: " + messageConstructor + " for " + idBig)
            complete(InternalServerError, "Request could not be completed: \n" + ex.getMessage)
        }
      }
    } catch {
      case e: NumberFormatException =>
        complete(BadRequest, "Numbers are formatted incorrectly")
    }
  }

  def genericPost(id: String, args: List[HttpHeader], messageConstructor: (BigInt, List[HttpHeader]) => PostInfo) = {
    try {
      val idBig = BigInt(id)
      pathEnd {
        onComplete(OnCompleteFutureMagnet(backbone ? messageConstructor.apply(idBig, args))) {
          case Success(newEntityJson) =>
            log.info("post completed successfully: " + messageConstructor + " for " + idBig)
            complete(newEntityJson)
          case Failure(ex) =>
            log.error(ex, "get failed: " + messageConstructor + " for " + idBig)
            complete(InternalServerError, "Request could not be completed: \n" + ex.getMessage)
        }
      }
    } catch {
      case e: NumberFormatException =>
        complete(BadRequest, "Numbers are formatted incorrectly")
    }
  }

  /**
   * same as above except no need to parse a special message since the path tells all and this is for putting, so the fully constructed message gets passed here
   * @param message constructed message to send to the backbone for handling
   * @return
   */
  def genericPut(message: PutInfo) = {
    pathEnd {
      onComplete(OnCompleteFutureMagnet(backbone ? message)) {
        case Success(newEntityJson) =>
          log.info("put completed successfully: " + message)
          complete(newEntityJson)
        case Failure(ex) =>
          log.error(ex, "put failed: " + message)
          complete(InternalServerError, "Error putting entity: " + ex.getMessage)
      }
    }
  }

  override def receive = runRoute(route)
}

object F_Listener {
  def props = ???
}


/*ideas for speed improvements:
parse out arguments before passing to backbone (might help with scaling to distributed system)
 */