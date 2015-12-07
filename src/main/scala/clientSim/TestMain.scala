package clientSim


import javax.net.ssl.{TrustManager, SSLContext}

import akka.actor.{ActorRef, ActorSystem, Props, _}
import akka.io.IO
import spray.can.Http
import system._
//import MatchClasses._
import clientSim.CaseClasses._
import spray.io.{ServerSSLEngineProvider, ClientSSLEngineProvider}
import util.DummyTrustManager


object TestMain {


  def main(args : Array[String]): Unit =
  {
    implicit val myClientEngineProvider = ClientSSLEngineProvider { engine =>
      engine.setEnabledCipherSuites(Array("TLS_RSA_WITH_AES_256_CBC_SHA"))
      engine.setEnabledProtocols(Array("SSLv3", "TLSv1"))
      engine
    }

    implicit val myServerEngineProvider = ServerSSLEngineProvider { engine =>
      engine.setEnabledCipherSuites(Array("TLS_RSA_WITH_AES_256_CBC_SHA"))
      engine.setEnabledProtocols(Array("SSLv3", "TLSv1"))
      engine
    }

    implicit val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(null, Array.fill[TrustManager](1)(new DummyTrustManager) , new java.security.SecureRandom())

    implicit val system = ActorSystem("TestActorSystem")

    val handler:ActorRef = system.actorOf(F_Server.props,"handler")
    IO(Http) ! Http.Bind(handler, "localhost", port = 8080)


    var userRef: ActorRef = system.actorOf(Props(new F_UserClient()), "UserClientActor")
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