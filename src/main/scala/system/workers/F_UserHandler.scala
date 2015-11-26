package system.workers

import java.util.{Date, MissingFormatArgumentException}

import akka.actor
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import graphnodes.F_User
import graphnodes.F_User._
import spray.http.{HttpRequest, Uri}
import system.F_BackBone._
import system.jsonFiles.F_UserJSON

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

import scala.language.postfixOps
import scala.xml.MalformedAttributeException

//TODO handle removing from friends friends lists when user deleted
//TODO add somethign to be able to remove friends

class F_UserHandler(backbone: ActorRef) extends Actor with ActorLogging {
  import F_UserHandler._
  import context.dispatcher

  implicit val timeout = Timeout(5 seconds)

  val users: collection.mutable.Map[BigInt, F_User] = collection.mutable.Map()

  def receive: Receive = {
    case CreateUser(request) =>
      createUser(request)

    case GetUserInfo(id) =>
      val replyTo = sender

      users.get(id) match {
        case Some(user) => Future(F_UserJSON.getJSON(user)) pipeTo replyTo
        case None => replyTo ! noSuchUserFailure(id)
      }

    case UpdateUserData(id, request) =>
      updateUserData(id, request)

    case DeleteUser(id) =>
      users.remove(id) match {
        case Some(user) =>
          backbone ! DeleteUserProfile(user.profileID)
          sender ! "User Deleted!"
        case None => sender ! noSuchUserFailure(id)
      }

    case RequestFriend(requesterID, request) =>
      requestFriend(requesterID, request)

    case HandleFriendRequest(acceptorID, request) =>
      handleFriendRequest(acceptorID, request)

    case RemoveFriend(removerID, request) =>
      removeFriend(removerID, request)
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
      val replyTo = sender
      Future(F_UserJSON.getJSON(newUser)) pipeTo replyTo
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
          case Some(value) =>
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

      val replyTo = sender

      Future(F_UserJSON.getJSON(updatedUser)) pipeTo replyTo
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }

  /**
   * handles the friend request logic
   * @param requesterID who requested
   * @param request contanins requestee
   */
  def requestFriend(requesterID: BigInt, request: HttpRequest) {
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
  }

  /**
   * accept or deny request
   * @param acceptorID acceptor
   * @param request requestor and acceptance contained within
   */
  def handleFriendRequest(acceptorID: BigInt, request: HttpRequest) {
    try {
      val requesterID = BigInt(request.uri.query.getOrElse(friendRequestString, throw new MalformedAttributeException("no friend request parameter!")), 16)
      val acceptedString = request.uri.query.getOrElse(acceptFriendString, throw new MalformedAttributeException("no acceptance parameter!"))
      val accepted = if (acceptedString == "true") true else false
      (users.get(acceptorID), users.get(requesterID)) match {
        case (Some(acceptor), Some(requester)) =>
          if (acceptor.friendRequests.contains(requesterID)) {
            //if this request actually occurred
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

  def removeFriend(removerID: BigInt, request: HttpRequest) = {
    try {
      val removedID = BigInt(request.uri.query.getOrElse(friendRemoveString, throw new MalformedAttributeException("no friend remove parameter!")), 16)
      (users.get(removerID), users.get(removedID)) match {
        case (Some(remover), Some(removed)) =>
          users.put(removerID, remover.copy(friends = remover.friends.filter(_ != removedID)))
          users.put(removedID, removed.copy(friends = removed.friends.filter(_ != removerID)))
        case (Some(remover), None) =>
          sender ! noSuchUserFailure(removedID)
        case (None, Some(removed)) =>
          sender ! noSuchUserFailure(removerID)
        case (None, None) =>
          sender ! noSuchUserFailure(removerID, removedID)
      }
    } catch {
      case ex: Exception =>
        sender ! actor.Status.Failure(ex)
    }
  }
}

object F_UserHandler {
  def props(backbone: ActorRef) = Props(new F_UserHandler(backbone))

  /**
   * exception to throw or put in messages when no such user
   * @param id id
   * @return exception
   */
  private def noSuchUserException(id: Seq[BigInt]) = new NoSuchElementException("there is no user entry for " + id.mkString(", "))

  /**
   * message to send when user id does not exist
   * @param ids id
   * @return message
   */
  private def noSuchUserFailure(ids: BigInt*) = actor.Status.Failure(noSuchUserException(ids))
}
