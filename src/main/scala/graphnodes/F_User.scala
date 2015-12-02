package graphnodes

import java.security.{Key, PublicKey}
import java.util.{Calendar, Date}
import javax.crypto.SecretKey
import F_User._

case class F_User(firstName: String, lastName: String, biography: String, age: Int,
                   dateOfBirth: Date, dateOfCreation: Date,
                   friends: List[(BigInt, SecretKey)], //each friend is now tupled with their public secret key encrypted with your own private key
                   friendRequests: List[BigInt],
                   profileID: BigInt,
                   userID: BigInt)

case class F_UserE(firstName: Array[Byte], lastName: Array[Byte], biography: Array[Byte], age: Array[Byte],
                   dateOfBirth: Array[Byte], dateOfCreation: Array[Byte],
                   friends: Array[Byte],
                   friendRequests: Array[Byte],
                   profileID: Array[Byte],
                   userID: BigInt)

case class F_UserES(userE: F_UserE, //server side F_User which contains some extra authentication info
                   identityKey: Option[PublicKey], //this is the general public key used for authenticaiton for this user, as wella s friend requesting, options are because this isnt on user side
                   authenticationAnswer: BigInt, //the answer to the authentication which was encrypted using the identity key and should come back decrypted to equal this
                   sessionAESCookieDecrypter: SecretKey, //when a session is established this aes key is used to decrypt the data in the cookie to be certain teh session is correct
                   sessionExpiration: Date = anHourFromNow) //the latest that the sessionAEScookie will work before authentication failed gets sent back, default is one hour later

object F_User {
  //these are needed in creation
  val lastNameString = "lastname"
  val firstNameString = "firstname"
  val bioString = "bio"
  val ageString = "age"
  val dobString = "dob"

  //these are used for friend request when doing a POST to edit the user, which is why they are in changable parameters
  //if friendrequest=true then it checks to acceptfriend for true and if so it accepts, otherwise it ignores and removes info from the user about the requester
  val friendRequestString = "friendrequest"
  val acceptFriendString = "acceptfriend"

  //This query is the only one needed when doing the uri for removing friends /users/remove/REMOVERID and this query's value contains the ID to remove
  val friendRemoveString = "remove"

  val changableParameters = List(lastNameString, firstNameString, bioString, ageString, dobString, friendRequestString, acceptFriendString)

  def anHourFromNow = {
    val cal = Calendar.getInstance; // creates calendar
    cal.setTime(new Date()); // sets calendar time/date
    cal.add(Calendar.HOUR_OF_DAY, 1); // adds one hour
    cal.getTime
  }

  implicit class UserEncrypter(user: F_User) {
    import util.Crypto._
    def encryptUser(key: Key) = {
      F_UserE(user.firstName.getBytes.encryptRSA(key), user.lastName.getBytes.encryptRSA(key), user.biography.getBytes.encryptRSA(key), user.age.toByteArray.encryptRSA(key),
        user.dateOfBirth.toByteArray.encryptRSA(key), user.dateOfCreation.toByteArray.encryptRSA(key), user.friends.toByteArray.encryptRSA(key), user.friendRequests.toByteArray.encryptRSA(key),
        user.profileID.toByteArray.encryptRSA(key), user.userID)
    }
  }

  implicit class UserDecrypter(user: F_UserE) {
    import util.Crypto._
    def decryptUserE(key: Key) = {
      F_User(user.firstName.decryptRSA(key).byteArray2String, user.lastName.decryptRSA(key).byteArray2String, user.biography.decryptRSA(key).byteArray2String, user.age.decryptRSA(key).byteArray2Int,
        user.dateOfBirth.decryptRSA(key).streamDeserializeToObject[Date], user.dateOfCreation.decryptRSA(key).streamDeserializeToObject[Date],
        user.friends.decryptRSA(key).streamDeserializeToObject[List[(BigInt, SecretKey)]], user.friendRequests.decryptRSA(key).streamDeserializeToObject[List[BigInt]],
        user.profileID.decryptRSA(key).streamDeserializeToObject[BigInt], user.userID)
    }
  }
}