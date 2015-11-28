package clientSim

import akka.actor._
import graphnodes._
import spray.json._
import spray.httpx.SprayJsonSupport


/*class F_AverageUserClient extends F_BaseClient {

  case class simulate()

  var user_ME = F_User(null, null, null, 0, null, null, null, null, null, null)
  var profile_ME = new F_UserProfile(null, null, null, null, null, null)

/*
  //from here: this may not be needed then, just use the object
  var firstName :String = "Ali"
  var lastName :String = "Gator"
  var biography :String = "UF_Gator"
  var age : Int = 21
  var ID :BigInt = 0
  var friendList :List[BigInt] = null
  var friendRequests : List[BigInt] = null
  var postList : List[String] = null
  //upto here
*/

  override def receive ={

    /case begin =>
      self ! createUser

    case userCreated(res) =>
      user_ME = res.toJson.convertTo[F_User]
      self ! simulate

    case simulate =>
      print("success to simulate")
      System.exit(1);


  }

}
*/