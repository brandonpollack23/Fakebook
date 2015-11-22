package system.workers

import java.security.SecureRandom
import java.util.{MissingFormatArgumentException, Date}

import akka.actor
import akka.pattern.ask
import graphnodes.F_User._

import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import spray.http.{Uri, HttpRequest}
import system.F_BackBone._
import graphnodes.F_User
import scala.collection.mutable.Map
import scala.concurrent.Await

import scala.concurrent.duration._

//TODO implement the logic for each transaction
class F_UserHandler(backbone: ActorRef) extends Actor with ActorLogging {
  import context.dispatcher

  val users: Map[BigInt, F_User] = Map()

  implicit val randomIDGenerator = new SecureRandom()

  def receive = {
    case CreateUser(request) =>
      createUser(request)

    case GetUserInfo(id) =>

  }

  def putUserInMap(id: BigInt) = ???

  def removeUserFromMap(id: BigInt) = ???

  /**
   * Creates a user by parsing parameters
   * @param httprequest contains arguments for user
   * @return replies to a future with the user JSON or with a status failure
   */
  def createUser(httprequest: HttpRequest) = {
    def createProfile(userID: BigInt) = {
      (backbone ? CreateUserProfile(userID)).mapTo[BigInt]
    }

    def getAllComponents(id: BigInt, params: Uri.Query) = {
      def extractComponent(key: String) = {
        params.find(_._1 == key).map(_._2) match {
          case Some(x) => x
          case None => throw new MissingFormatArgumentException("there is no entry for " + key)
        }
      }

      val profileIDF = createProfile(id)

      (extractComponent(firstNameString), extractComponent(lastNameString), extractComponent(bioString), extractComponent(ageString).toInt,
        dateFormatter.parse(extractComponent(dobString)), /*date of creation*/new Date, /*empty friends list*/ List[BigInt](), /*no friend requests*/ List[BigInt](),
        Await.result(profileIDF, 5 seconds), /*userid*/ id)
    }

    val id = getUniqueRandomBigInt

    val params = httprequest.uri.query

    try {
      val newUser = (F_User.apply _).tupled(getAllComponents(id, params))
      users.put(id, newUser)
      sender ! newUser //TODO change to JSON
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def getUniqueRandomBigInt: BigInt = {
    def isUnique(x: BigInt) = !users.contains(x)

    val x = BigInt(256, randomIDGenerator) //use 256 bits b/c sha256 does so that is low on collisions right?
    if(isUnique(x)) x
    else getUniqueRandomBigInt
  }
}

object F_UserHandler {
  def props(backbone: ActorRef) = Props(new F_UserHandler(backbone))
}
