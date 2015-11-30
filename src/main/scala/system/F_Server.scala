package system

import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import akka.util.Timeout
import spray.can.Http

import scala.concurrent.duration._
import scala.language.postfixOps

class F_Server extends Actor with ActorLogging {
  implicit val timeout: Timeout = 1 second

  val backbone: ActorRef = context.actorOf(F_BackBone.props)

  def receive = {
    case _: Http.Connected =>
      val httphandler = context.actorOf(F_Listener.props(backbone, sender()))
      log.info("received connection from " + sender().path.address.host)
      sender ! Http.Register(httphandler)
    case _ =>
      log.error("unrecognized message")

      //if distributed add messages to take in new backbones etc
  }
}

object F_Server {
  def props = Props[F_Server]
}
