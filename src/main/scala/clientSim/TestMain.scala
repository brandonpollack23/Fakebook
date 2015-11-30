package clientSim


import akka.actor.{Props, ActorRef, ActorSystem}
import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import akka.testkit.TestActorRef
import akka.util.Timeout
import clientSim.MatchClasses.{Simulate, Begin}
import system.workers.{F_PageProfileHandler, F_UserHandler, F_PictureHandler}
import system._
import spray.http._
import akka.io.IO
import spray.can.Http
import spray.json._
import DefaultJsonProtocol._
import akka.io.IO

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
import MyJsonProtocol._
import scala.concurrent.duration._
import java.text.SimpleDateFormat
import java.util.Date
import MatchClasses._


object TestMain {


  def main(args : Array[String]): Unit =
  {
    implicit val system = ActorSystem("TestActorSystem")

    val handler:ActorRef = system.actorOf(F_Server.props,"handler")
    IO(Http) ! Http.Bind(handler, "localhost", port = 8080)


    implicit val system1 = ActorSystem("ActorSystem")

    val userLoad :Int= 1000
    val heavyPercent:Double = 0.1
    val lightPercent:Double = 0.4
    val normalPercent :Double= 0.5

    val heavy:Int = (userLoad * heavyPercent).toInt
    val light:Int = (userLoad * lightPercent).toInt
    val normal:Int = (userLoad * normalPercent).toInt

    var i:Int = 0
    while(i<heavy) {

      var userRef: ActorRef = system1.actorOf(Props(new F_HeavyUserClient()), "user_client"+i)
      userRef ! Begin
      i += 1
    }
    i=0

    while(i<light){
      var userRef: ActorRef = system1.actorOf(Props(new F_LightUserClient()), "user_client"+i)
      userRef ! Begin
      i += 1

    }
      i=0

    while(i<normal){

      var userRef: ActorRef = system1.actorOf(Props(new F_AverageUserClient()), "user_client"+i)
      userRef ! Begin
      i += 1
    }



 }

}