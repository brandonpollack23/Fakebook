package system.workers

import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import system.F_BackBone._

//TODO implement the logic for each transaction
class F_PictureHandler(backbone: ActorRef) extends Actor with ActorLogging {
  def receive = ???
}

object F_PictureHandler {
  def props(backbone: ActorRef) = Props(new F_PictureHandler(backbone))
}