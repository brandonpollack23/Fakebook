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

class F_Listener(backbone: ActorRef) extends Actor with ActorLogging with HttpService {
  implicit val timeout = Timeout(5 seconds)

  val route: Route = {
    val requester = sender
    path("data") { id => //URIs for actual data like pictures
      get {
        genericGet(id.unmatchedPath.toString, GetImage)
      } 
    } ~
    path("user") { id =>
      get {
        genericGet(id.unmatchedPath.toString, GetUserInfo)
      }
    } ~
    path("page") { id =>
      get {
        genericGet(id.unmatchedPath.toString, GetPageInfo)
      }
    } ~
    path("profile") { id =>
      get {
        genericGet(id.unmatchedPath.toString, GetProfileInfo)
      }
    } ~
    path("picture") { id =>
      get {
        genericGet(id.unmatchedPath.toString, GetPictureInfo)
      }
    } ~
    path("album") { id =>
      get {
        genericGet(id.unmatchedPath.toString, GetAlbumInfo)
      }
    }
  }

  def genericGet[T](id: String, messageConstructor: (BigInt) => GetInfo): Route = {
    try {
      val idBig = BigInt(id)
      onComplete[T](OnCompleteFutureMagnet[T]((backbone ? messageConstructor.apply(idBig)).mapTo[T])) {
        case Success(x) => complete(x)
        case Failure(ex) => complete(InternalServerError, "Request could not be completed: \n" + ex.getMessage)
      }
    } catch {
      case e: NumberFormatException =>
        complete(BadRequest, "Numbers are formatted incorrectly")
    }
  }

  def receive = runRoute(route)

  override implicit def actorRefFactory: ActorRefFactory = context
}

object F_Listener {
  def props = ???
}
