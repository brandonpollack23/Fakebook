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

    val totalUsers = 100

    val userRef: ActorRef = system.actorOf(Props(new F_MasterClient(totalUsers)), "MasterClientActor")
    userRef ! Begin




    /*
    var i = 0

    while (i < 10) {
    var userRef: ActorRef = system.actorOf(Props(new F_UserClient(i)), "UserClientActor" + i)
    userRef ! Begin
      i += 1
  }

    val userLoad :Int= 100
    val heavyPercent:Double = 0.05
    val lightPercent:Double = 0.35
    val normalPercent :Double= 0.6

    val heavy:Int = (userLoad * heavyPercent).toInt
    val light:Int = (userLoad * lightPercent).toInt
    val normal:Int = (userLoad * normalPercent).toInt

    var i:Int = 0
    var userRef: ActorRef = null



    while(i<heavy) {

      userRef = system1.actorOf(Props(new F_HeavyUserClient()), "heavy_user_client"+i)
      userRef ! Begin
      i += 1
    }
    i=0

    while(i<light){
      userRef = system1.actorOf(Props(new F_LightUserClient()), "light_user_client"+i)
      userRef ! Begin
      i += 1

    }
      i=0

    while(i<normal){

      userRef = system1.actorOf(Props(new F_AverageUserClient()), "normal_user_client"+i)
      userRef ! Begin
      i += 1
    }

*/

 }

}