package clientSim

import akka.actor.{ActorRef, ActorSystem, Props, _}
import akka.io.IO
import clientSim.CaseObjects.{Begin, GetAuthCode}
import spray.can.Http
import system._


object TestMain {


  def main(args : Array[String]): Unit = {

    implicit val system = ActorSystem("TestActorSystem")

    val handler: ActorRef = system.actorOf(F_Server.props, "handler")
    IO(Http) ! Http.Bind(handler, "localhost", port = 8080)



    val totalUsers = 50



    implicit val system1 = ActorSystem("ClientActorSystem")

    val userRef: ActorRef = system1.actorOf(Props(new F_MasterClient(totalUsers)), "MasterClientActor")
    userRef ! Begin



 }

}