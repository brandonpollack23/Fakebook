package system

import akka.actor.{ActorRef, Actor, ActorLogging}
import akka.util.Timeout
import spray.can.Http
import system.workers._

import scala.concurrent.duration._
import scala.language.postfixOps

class F_Server extends Actor with ActorLogging {
  implicit val timeout: Timeout = 1 second

  val backbone: ActorRef = context.actorOf(F_BackBone.props(context.actorOf(F_PictureHandler.props(backbone)), context.actorOf(F_UserHandler.props(backbone)), context.actorOf(F_PageProfileHandler.props(backbone))))

  def receive = {
    case _: Http.Connected =>
      val httphandler = context.actorOf(F_Listener.props(backbone))
      log.info("received connection from " + sender.path.address.host)
      sender ! Http.Register(httphandler)

      //if distributed add messages to take in new backbones etc
  }
}
