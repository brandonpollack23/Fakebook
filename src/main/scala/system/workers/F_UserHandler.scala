package system.workers

import java.security.MessageDigest
import java.util.Date

import akka.actor
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import graphnodes.F_User._
import graphnodes.{F_User, F_UserE, F_UserES}
import spray.http.HttpHeaders.Cookie
import spray.http.StatusCodes._
import spray.http._
import spray.json._
import system.F_BackBone._
import util.Crypto._
import util.MyJsonProtocol._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps
import scala.xml.MalformedAttributeException

class F_UserHandler(backbone: ActorRef) extends Actor with ActorLogging {
  import F_UserHandler._
  import context.dispatcher

  implicit val timeout = Timeout(5 seconds)

  val users: collection.mutable.Map[BigInt, F_UserES] = collection.mutable.Map()

  val md = MessageDigest.getInstance("SHA-256")

  //TODO check all creates and make sure default stuff is overriden that should be (eg no friends on creation)
  def receive: Receive = {
    case GetUserList =>
      val replyTo = sender()
      Future(users.keys.toJson.compactPrint) pipeTo replyTo

    case CreateUser(request) =>
      createUser(request)

    case GetUserInfo(id) =>
      val replyTo = sender()

      users.get(id) match {
        case Some(user) => Future(user.userE.toJson.compactPrint) pipeTo replyTo
        case None => replyTo ! noSuchUserFailure(id)
      }

    case UpdateUserData(id, request) =>
      updateUserData(id, request)

    case DeleteUser(id) =>
      try {
        users.remove(id) match {
          case Some(user) =>
            user.userE.friends.foreach { userTuple =>
              val friendID = userTuple._1
              val friend = users.getOrElse(friendID, throw noSuchUserException(List(friendID)))
              users.put(friendID, friend.copy(userE = friend.userE.copy(friends = friend.userE.friends.filter(_._1 != id)))) //remove user from friend's friends
            }
            backbone ! DeleteUserProfile(user.userE.profileID)
            sender ! "User Deleted!"
          case None => sender ! noSuchUserFailure(id)
        }
      } catch {
        case ex: Exception =>
          sender ! actor.Status.Failure(ex)
      }

    case RequestFriend(requesterID, request) => //TODO make sure this is handled right
      requestFriend(requesterID, request)

    case HandleFriendRequest(acceptorID, request) =>
      handleFriendRequest(acceptorID, request)

    case RemoveFriend(removerID, request) =>
      removeFriend(removerID, request)

    case SetUpAuthenticateUser(id, request) =>
      setUpAuthenticateUser(id)

    case VerifyAuthenticationCookie(id, cookie) =>
      verifyAuthenticationCookie(id, cookie)
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

    val userID = getUniqueRandomBigInt(users)

    try {
      val profileID = createProfile(userID)
      val newUser = request.entity.asString.parseJson.convertTo[F_UserE].copy(dateOfCreation = new Date, friends = List[(BigInt, Array[Byte])](), userID = userID, friendRequests = List[(BigInt, Array[Byte])](),
        profileID = Await.result(profileID, 5 seconds))
      users.put(userID, F_UserES(newUser, BigInt(0), new Date))
      val replyTo = sender()
      Future(newUser.toJson.compactPrint).mapTo[String] pipeTo replyTo
    } catch {
      case ex: Exception =>
        log.error("user error: " + ex + " " + ex.getCause)
        sender ! actor.Status.Failure(ex)
    }
  }

  /**
   * update a users info based on what parameters may be in the request
   * @param id id of user to update
   * @param request request
   */
  def updateUserData(id: BigInt, request: HttpRequest) {
    def updateCurrentUserES(user: F_UserES, fields: Map[String, JsValue]): F_UserES = {
     def updateCurrentUser(user: F_UserE, fields: Map[String, JsValue]): F_UserE = {
        if (fields.isEmpty) user
        else {
          val currentParameter = fields.head
          currentParameter._1 match {
            case F_User.`lastNameField` => updateCurrentUser(user.copy(lastName = currentParameter._2.convertTo[Array[Byte]]), fields.tail)
            case `firstNameField` => updateCurrentUser(user.copy(firstName = currentParameter._2.convertTo[Array[Byte]]), fields.tail)
            case `bioField` => updateCurrentUser(user.copy(biography = currentParameter._2.convertTo[Array[Byte]]), fields.tail)
            case `ageField` => updateCurrentUser(user.copy(age = currentParameter._2.convertTo[Array[Byte]]), fields.tail)
            case `dobField` => updateCurrentUser(user.copy(dateOfBirth = currentParameter._2.convertTo[Array[Byte]]), fields.tail)
            case _ => updateCurrentUser(user, fields.tail)
          }
        }
      }

      user.copy(userE = updateCurrentUser(user.userE, fields))
    }

    try{
      val user = users.getOrElse(id, throw noSuchUserException(List(id)))

      val fields = request.entity.asString.parseJson.asJsObject.fields

      val updatedUser = updateCurrentUserES(user, fields)

      users.put(id, updatedUser)

      val replyTo = sender()

      Future(updatedUser.userE.toJson.compactPrint).mapTo[String] pipeTo replyTo
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
      val requestedFriendID = BigInt(request.uri.query.getOrElse(friendRequestString, throw new MalformedAttributeException("no friendrequest parameter!")), 16)
      (users.get(requesterID), users.get(requestedFriendID)) match {
        case (Some(requesterS), Some(requestedS)) =>
          val requested = requestedS.userE
          val requestedID = requested.userID
          val encryptedAES = request.entity.asString.parseJson.convertTo[Array[Byte]]
          users.put(requestedID, requestedS.copy(userE = requested.copy(friendRequests = (requesterID, encryptedAES) :: requested.friendRequests)))
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
        case (Some(acceptorS), Some(requesterS)) =>
          val requester = requesterS.userE
          val acceptor = acceptorS.userE
          val requesterTupleO = acceptor.friendRequests.find(_._1 == requesterID)
          val acceptorTuple = (acceptorID, request.entity.asString.parseJson.convertTo[Array[Byte]])
          requesterTupleO match {
            case Some(requesterTuple) =>
              if (accepted) { //if this request actually occurred
                users.put(acceptorID, acceptorS.copy(userE = acceptor.copy(friends = requesterTuple :: acceptor.friends))) //add to each others friends lists
                users.put(requesterID, requesterS.copy(userE = requester.copy(friends = acceptorTuple :: requester.friends)))
                sender ! "Friend Accepted!"
              }
              else {
                users.put(acceptorID, acceptorS.copy(userE = acceptor.copy(friendRequests = acceptor.friendRequests.filter(_ != requesterID)))) //remove requst from list
                sender ! "Friend Denied!"
              }
            case None =>
              sender ! actor.Status.Failure(new IllegalArgumentException("No such friend has requested you"))
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
        case (Some(removerS), Some(removedS)) =>
          val remover = removerS.userE
          val removed = removedS.userE
          users.put(removerID, removerS.copy(userE = remover.copy(friends = remover.friends.filter(_ != removedID))))
          users.put(removedID, removedS.copy(userE = removed.copy(friends = removed.friends.filter(_ != removerID))))
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

  /**
   * This begins user authentication by sending them an SRNG number to encrypt for hte server to decrypt
   * and storing that in the user's F_UserES object for later reference
   * @param id user id
   */
  def setUpAuthenticateUser(id: BigInt) {
    val solutionNumber = BigInt(256, randomIDGenerator) // generate a number
    users.get(id) match {
      case Some(us) =>
        val solutionHash = BigInt(md.digest(solutionNumber.toByteArray))
        users.put(id, us.copy(sessionExpiration = F_User.anHourFromNow, authenticationAnswerHash = solutionHash))
        val problemNumber = BigInt(solutionNumber.toByteArray.encryptRSA(us.userE.identityKey)).toString(16)
        val cookie = Cookie(HttpCookie(authenticationCookieName, problemNumber/*, secure = true*/)) //when SSL is enabled secure should be true
        sender ! HttpResponse(OK, headers = List(cookie)) //respond with cookie TODO on user side this cookie needs to be decrypted with private identity key and remade using decryption
      case None =>
        sender ! noSuchUserFailure(id)
    }
  }

  def verifyAuthenticationCookie(id: BigInt, cookie: HttpCookie) {
    try {
      (users.get(id), BigInt(cookie.content, 16).toByteArray) match {
        case (Some(user), aesKeyBytes) =>
          val solutionHash = BigInt(md.digest(aesKeyBytes))
          if (user.sessionExpiration after new Date) {
            if (solutionHash == user.authenticationAnswerHash) sender ! true //just send any kind of success
            else sender ! actor.Status.Failure(new Exception("cookie key does not match with session"))
          } else {
            sender ! actor.Status.Failure(new Exception("your session has expired, please reverify"))
          }
        case (None, _) =>
          sender ! noSuchUserFailure(id)
      }
    } catch {
      case ex: NumberFormatException =>
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
  private def noSuchUserException(id: Seq[BigInt]) = new NoSuchElementException("there is no user entry for " + id.map(_.toString(16)).mkString(", "))

  /**
   * message to send when user id does not exist
   * @param ids id
   * @return message
   */
  private def noSuchUserFailure(ids: BigInt*) = actor.Status.Failure(noSuchUserException(ids))
}
