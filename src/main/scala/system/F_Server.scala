package system

import akka.actor.Actor.Receive
import akka.actor.{Actor, ActorLogging}
import akka.util.Timeout
import spray.can.Http

import scala.concurrent.duration._

class F_Server extends Actor with ActorLogging {
  implicit val timeout: Timeout = 1 second

  val backbone = context.actorOf(F_BackBone.props)

  def receive = {
    case _: Http.Connected =>
      val httphandler = context.actorOf(F_Listener.props(backbone))
      log.info("received connection from " + sender.path.address.host)
      sender ! Http.Register(httphandler)
  }
}
