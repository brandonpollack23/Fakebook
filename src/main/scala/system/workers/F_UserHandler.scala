package system.workers

import java.security.SecureRandom
import java.util.{MissingFormatArgumentException, Date}

import akka.actor
import akka.pattern.ask
import akka.util.Timeout
import graphnodes.F_User._

import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import spray.http.{Uri, HttpRequest}
import system.F_BackBone._
import graphnodes.F_User
import scala.annotation.tailrec
import scala.collection.mutable.Map
import scala.concurrent.Await

import scala.concurrent.duration._

//TODO implement the logic for each transaction
class F_UserHandler(backbone: ActorRef) extends Actor with ActorLogging {
  import context.dispatcher

  implicit val timeout = Timeout(5 seconds)

  val users: Map[BigInt, F_User] = Map()

  def receive = {
    case CreateUser(request) =>
      createUser(request)

    case GetUserInfo(id) =>
      users.get(id) match {
        case Some(user) => sender ! user //TODO change to JSON
        case None => sender ! noSuchUserFailure(id)
      }

    case UpdateUserData(id, request) =>
      updateUserData(id, request)
  }

  /**
   * Creates a user by parsing parameters
   * @param request contains arguments for user
   * @return replies to a future with the user JSON or with a status failure
   */
  def createUser(request: HttpRequest) {
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

    val id = getUniqueRandomBigInt(users)

    val params = request.uri.query

    try {
      val newUser = (F_User.apply _).tupled(getAllComponents(id, params))
      users.put(id, newUser)
      sender ! newUser //TODO change to JSON
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  def updateUserData(id: BigInt, request: HttpRequest) {
    @tailrec
    def updateCurrentUserInstance(user: F_User, params: Uri.Query, parametersRemaining: List[String]): F_User = {
      if(parametersRemaining.isEmpty) {
        user
      } else {
        val currentParameter = parametersRemaining.head
        params.get(currentParameter) match {
          case Some(value) =>
            if(currentParameter == lastNameString) updateCurrentUserInstance(user.copy(lastName = value), params, parametersRemaining.tail)
            else if(currentParameter == firstNameString) updateCurrentUserInstance(user.copy(firstName = value), params, parametersRemaining.tail)
            else if(currentParameter == bioString) updateCurrentUserInstance(user.copy(biography = value), params, parametersRemaining.tail)
            else if(currentParameter == ageString) updateCurrentUserInstance(user.copy(age = value.toInt), params, parametersRemaining.tail)
            else if(currentParameter == dobString) updateCurrentUserInstance(user.copy(dateOfBirth = dateFormatter.parse(value)), params, parametersRemaining.tail)
            else throw new IllegalArgumentException("there is no case to handle such parameter in the list (system issue)")
          case None =>
            updateCurrentUserInstance(user, params, parametersRemaining.tail)
        }
      }
    }

    val user = users.get(id) match {
      case Some(user) => user
      case None =>
        sender ! noSuchUserFailure(id)
        return
    }

    val params = request.uri.query


      val updatedUser = try {
        updateCurrentUserInstance(user, params, changableParameters)
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
        return
    }

    users.put(id, updatedUser)

    sender ! updatedUser //TODO make JSON
  }

  def noSuchUserFailure(id: BigInt) = actor.Status.Failure(new MissingFormatArgumentException("there is no entry for " + id))
}

object F_UserHandler {
  def props(backbone: ActorRef) = Props(new F_UserHandler(backbone))
}
