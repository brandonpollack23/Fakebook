package system

import akka.actor.{ActorRef, ActorRefFactory, ActorLogging, Actor}
import akka.util.Timeout
import spray.routing.directives.OnCompleteFutureMagnet
import spray.routing.{HttpService, Route}
import spray.http.StatusCodes._

import scala.concurrent.duration._

import akka.pattern.ask

import system.F_BackBone._

import scala.util.{Failure, Success}

//TODO log everything

class F_Listener(backbone: ActorRef) extends Actor with ActorLogging with HttpService {
  implicit val timeout = Timeout(5 seconds)

  val route: Route = {
    val requester = sender
    path("data") {
      get { id => //URIs for actual data like pictures
        genericGet(id.unmatchedPath.toString, GetImage)
      } ~
      path("uploadimage") {
        put { req =>
          genericPut(PutImage(req.request.entity))
        }
      }
    } ~
    path("user") {
      get { id =>
        genericGet(id.unmatchedPath.toString, GetUserInfo)
      } ~
      path("newuser") {
        put { req =>
            genericPut(CreateUser(req.request))
        }
      }
    } ~
    path("page") { id =>
      get {
        genericGet(id.unmatchedPath.toString, GetPageInfo)
      } ~
      path("newpage") {
        put { req =>
          genericPut(CreatePage(req.request))
        }
      }
    } ~
    path("profile") { id =>
      get {
        genericGet(id.unmatchedPath.toString, GetProfileInfo)
      } //no need for "createprofile" they are created with the user and can be accessed through the JSON returned with that creation
    } ~
    path("picture") { id =>
      get {
        genericGet(id.unmatchedPath.toString, GetPictureInfo)
      } //same as for profile, when you upload an image the picture JSON is created
    } ~
    path("album") { id =>
      get {
        genericGet(id.unmatchedPath.toString, GetAlbumInfo)
      } ~
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
   * @param id the string to be converted to bigint as an ID
   * @param messageConstructor the case class message (constructor but can be passed with sugar) to compose the bigint into
   * @return returns a route for thed spray routing to go through and side effects the complete needed
   */
  def genericGet(id: String, messageConstructor: (BigInt) => GetInfo): Route = {
    try {
      val idBig = BigInt(id)
      onComplete(OnCompleteFutureMagnet(backbone ? messageConstructor.apply(idBig))) {
        case Success(x) => complete(x)
        case Failure(ex) => complete(InternalServerError, "Request could not be completed: \n" + ex.getMessage)
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
        case Success(newEntityJson) => complete(newEntityJson)
        case Failure(ex) => complete(InternalServerError, "Error putting entity: " + ex.getMessage)
      }
    }
  }

  def receive = runRoute(route)

  override implicit def actorRefFactory: ActorRefFactory = context
}

object F_Listener {
  def props = ???
}
