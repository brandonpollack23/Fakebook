package system.workers

import java.util.{Date, MissingFormatArgumentException}

import akka.actor
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import graphnodes.F_User
import graphnodes.F_User._
import spray.http.{HttpRequest, Uri}
import system.F_BackBone._

import scala.concurrent.Await
import scala.concurrent.duration._

import scala.language.postfixOps
import scala.xml.MalformedAttributeException

class F_UserHandler(backbone: ActorRef) extends Actor with ActorLogging {

  implicit val timeout = Timeout(5 seconds)

  val users: collection.mutable.Map[BigInt, F_User] = collection.mutable.Map()

  def receive: Receive = {
    case CreateUser(request) =>
      createUser(request)

    case GetUserInfo(id) =>
      users.get(id) match {
        case Some(user) => sender ! user //TODO change to JSON
        case None => sender ! noSuchUserFailure(id)
      }

    case UpdateUserData(id, request) =>
      updateUserData(id, request)

    case DeleteUser(id) =>
      users.remove(id) match {
        case Some(user) => sender ! "User Deleted!"
        case None => sender ! noSuchUserFailure(id)
      }

    case RequestFriend(requesterID, request) =>
      try {
        val requestedFriendID = BigInt(request.uri.query.getOrElse(friendRequestString, throw new MalformedAttributeException("no friendrequest parameter!")))
        (users.get(requesterID), users.get(requestedFriendID)) match {
          case (Some(requester), Some(requested)) =>
            users.put(requesterID, requested.copy(friendRequests = requesterID :: requested.friendRequests))
            sender ! "Friend Request Sent!"
          case (Some(_), None) =>
            sender ! noSuchUserFailure(requestedFriendID)
          case (None, Some(_)) =>
            sender ! noSuchUserFailure(requesterID)
          case (None, None) =>
            sender ! noSuchUserFailure(requesterID, requestedFriendID)
        }
      } catch {
        case ex: Exception =>
          sender ! actor.Status.Failure(ex)
      }

    case HandleFriendRequest(acceptorID, request) =>
      try {
        val requesterID = BigInt(request.uri.query.getOrElse(friendRequestString, throw new MalformedAttributeException("no friend request parameter!")))
        val acceptedString = request.uri.query.getOrElse(acceptFriendString, throw new MalformedAttributeException("no acceptance parameter!"))
        val accepted = if(acceptedString == "true") true else false
        (users.get(acceptorID), users.get(requesterID)) match {
          case (Some(acceptor), Some(requester)) =>
            if(acceptor.friendRequests.contains(requesterID)) { //if this request actually occurred
              if (accepted) {
                users.put(acceptorID, acceptor.copy(friends = requesterID :: acceptor.friends)) //add to each others friends lists
                users.put(requesterID, requester.copy(friends = acceptorID :: requester.friends))
                sender ! "Friend Accepted!"
              } else {
                users.put(acceptorID, acceptor.copy(friendRequests = acceptor.friendRequests.filter(_ != requesterID))) //remove requst from list
                sender ! "Friend Denied!"
              }
            }
          case (Some(_), None) =>
            sender ! noSuchUserFailure(requesterID)
          case (None, Some(_)) =>
            sender ! noSuchUserFailure(acceptorID)
          case (None, None) =>
            sender ! noSuchUserFailure(acceptorID, requesterID)
        }
      } catch {
        case ex: Exception =>
          sender ! actor.Status.Failure(ex)
      }
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

  /** TODO place restrictions on certain changes
   * update a users info based on what parameters may be in the request
   * @param id id of user to update
   * @param request request
   */
  def updateUserData(id: BigInt, request: HttpRequest) {
    def updateCurrentUserInstance(user: F_User, params: Uri.Query, parametersRemaining: List[String]): F_User = {
      if(parametersRemaining.isEmpty) {
        user
      } else {
        val currentParameter = parametersRemaining.head
        params.get(currentParameter) match {
          case Some(value) => //TODO make swittch statement with ` `
            currentParameter match {
              case `lastNameString` => updateCurrentUserInstance(user.copy(lastName = value), params, parametersRemaining.tail)
              case `firstNameString` => updateCurrentUserInstance(user.copy(firstName = value), params, parametersRemaining.tail)
              case `bioString` => updateCurrentUserInstance(user.copy(biography = value), params, parametersRemaining.tail)
              case `ageString` => updateCurrentUserInstance(user.copy(age = value.toInt), params, parametersRemaining.tail)
              case `dobString` => updateCurrentUserInstance(user.copy(dateOfBirth = dateFormatter.parse(value)), params, parametersRemaining.tail)
              case _ => throw new IllegalArgumentException("there is no case to handle such parameter in the list (system issue)")
            }
          case None =>
            updateCurrentUserInstance(user, params, parametersRemaining.tail)
        }
      }
    }

    try{
      val user = users.getOrElse(id, throw noSuchUserException(List(id)))

      val params = request.uri.query


      val updatedUser = updateCurrentUserInstance(user, params, changableParameters)

      users.put(id, updatedUser)

      sender ! updatedUser //TODO make JSON
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  /**
   * exception to throw or put in messages when no such user
   * @param id id
   * @return exception
   */
  def noSuchUserException(id: Seq[BigInt]) = new NoSuchElementException("there is no entry for " + id.mkString(", "))

  /**
   * message to send when user id does not exist
   * @param ids id
   * @return message
   */
  def noSuchUserFailure(ids: BigInt*) = actor.Status.Failure(noSuchUserException(ids))
}

object F_UserHandler {
  def props(backbone: ActorRef) = Props(new F_UserHandler(backbone))
}
