package clientSim


import java.security.{KeyPair, KeyPairGenerator}
import java.util.Date
import javax.crypto.{KeyGenerator, SecretKey}
import util.MyJsonProtocol
import akka.actor.{Props, ActorRef, ActorSystem}
import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import graphnodes.F_User
import system._
import spray.can.Http
import akka.io.IO
import akka.actor._
import akka.actor.ActorSystem
//import MatchClasses._
import CaseClasses._

object TestMain {


  def main(args : Array[String]): Unit =
  {
    implicit val system1 = ActorSystem("TestActorSystem")

    val handler:ActorRef = system1.actorOf(F_Server.props,"handler")
    IO(Http) ! Http.Bind(handler, "localhost", port = 8080)
    val kGen: KeyGenerator = KeyGenerator.getInstance("AES");
    kGen.init(256);
    val aesKey: SecretKey = kGen.generateKey();

    //RSA Encryption
    val kpg: KeyPairGenerator = KeyPairGenerator.getInstance("RSA");
    kpg.initialize(2048);
    val kp: KeyPair = kpg.genKeyPair();
    val publicKey  = kp.getPublic();
    val privateKey = kp.getPrivate();

   // var user_ME    = F_User("Ali", "Gator", "Student at UF", 25, new Date(1989-1900,1,1), new Date(), List[(BigInt, SecretKey)](), List[(BigInt, Array[Byte])](), 0, publicKey, 0)
    //var x = user_ME.encryptUser(aesKey)
    //var y = user_ME

    //implicit val system1 = ActorSystem("ActorSystem")

      var userRef: ActorRef = system1.actorOf(Props(new F_UserClient()), "ClientUserActor")
      userRef ! Begin


    /*
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