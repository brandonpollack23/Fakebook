package system.workers

import java.security.SecureRandom

import akka.actor.{ActorLogging, Actor}
import spray.http.HttpRequest
import system.F_BackBone._
import graphnodes.F_User
import scala.collection.mutable.Map

//TODO implement the logic for each transaction
class F_UserHandler extends Actor with ActorLogging {
  val users: Map[BigInt, F_User] = Map()

  implicit val randomIDGenerator = new SecureRandom()

  def receive = {
    case GetUserInfo(id) =>

  }

  def putUserInMap(id: BigInt) = ???

  def removeUserFromMap(id: BigInt) = ???

  def createUser(httprequest: HttpRequest) = ??? //TODO use random to create a userid, make sure it isn't taken, then create the object, add it to the system and reply to sender with the JSON, JSON should contain ID as hex

  def getRandomBigInt = BigInt(256, randomIDGenerator) //use 256 bits b/c sha256 does so that is low on collisons right?
}
