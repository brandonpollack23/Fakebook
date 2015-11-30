package clientSim

//import _root_.MyJsonProtocol._
import akka.actor.{Props, ActorRef, ActorSystem}
import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import akka.testkit.TestActorRef
import akka.util.Timeout
import clientSim.MatchClasses.{Simulate, Begin}
import system.jsonFiles.F_UserJSON
import system.workers.{F_PageProfileHandler, F_UserHandler, F_PictureHandler}
import system.{F_Listener, F_BackBone, F_Server}
import spray.http._
import akka.io.IO
import spray.can.Http
import spray.json._
import DefaultJsonProtocol._
import akka.io.IO
import system.F_Server

import graphnodes._
import akka.actor._
import akka.util.Timeout
import akka.actor.Actor
import akka.actor.ActorSystem
import spray.http._
import spray.client.pipelining._
import spray.json.AdditionalFormats
import spray.httpx.SprayJsonSupport
import spray.client.pipelining.sendReceive
import scala.util.{Success, Failure}
import system.jsonFiles.MyJsonProtocol._
import scala.concurrent.duration._
import java.text.SimpleDateFormat
import java.util.Date
import MatchClasses._


object TestMain {


  def main(args : Array[String]): Unit =
  {
    implicit val system = ActorSystem("TestActorSystem")
    val baseRef: ActorRef = system.actorOf(Props[F_BaseClient], "base_client")
    var userRef : ActorRef = system.actorOf(Props(new F_AverageUserClient(baseRef)), "user_client")
    val handler:ActorRef = system.actorOf(F_Server.props,"handler")
    IO(Http) ! Http.Bind(handler, "localhost", port = 8080)

    userRef ! Begin

 }

}