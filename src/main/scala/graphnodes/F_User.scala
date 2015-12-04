package graphnodes

import java.security.{Key, PublicKey}
import java.util.{Calendar, Date}
import javax.crypto.SecretKey
import util.Crypto._
import F_User._
case class F_User(firstName: String, lastName: String, biography: String, age: Int,
                   dateOfBirth: Date, dateOfCreation: Date,
                   friends: List[(BigInt, BigInt)], //each friend is now tupled with their friend secret key encrypted with your own identity private key, so this key is user's RSA_pu(friendAES)
                   friendRequests: List[BigInt],
                   profileID: BigInt,
                   identityKey: PublicKey, //this is the general public key used for authenticaiton for this user, as wella s friend requesting, options are because this isnt on user side
                   userID: BigInt) {
  def encryptUser(key: Key) = {
    F_UserE(firstName.getBytes.encryptAES(key), lastName.getBytes.encryptAES(key), biography.getBytes.encryptAES(key), age.toByteArray.encryptAES(key),
      dateOfBirth.toByteArray.encryptAES(key), dateOfCreation.toByteArray.encryptAES(key), friends.toByteArray.encryptAES(key), friendRequests.toByteArray.encryptAES(key),
      profileID.toByteArray.encryptAES(key), identityKey, userID)
  }
}

case class F_UserE(firstName: Array[Byte], lastName: Array[Byte], biography: Array[Byte], age: Array[Byte],
                   dateOfBirth: Array[Byte], dateOfCreation: Array[Byte],
                   friends: Array[Byte],
                   friendRequests: Array[Byte],
                   profileID: Array[Byte],
                   identityKey: PublicKey, //this is the general public key used for authenticaiton for this user, as wella s friend requesting, options are because this isnt on user side
                   userID: BigInt) {
  def decryptUserE(key: Key) = {
    F_User(firstName.decryptAES(key).byteArray2String, lastName.decryptAES(key).byteArray2String, biography.decryptAES(key).byteArray2String, age.decryptAES(key).byteArray2Int,
      dateOfBirth.decryptAES(key).streamDeserializeToObject[Date], dateOfCreation.decryptAES(key).streamDeserializeToObject[Date],
      friends.decryptAES(key).streamDeserializeToObject[List[(BigInt, BigInt)]], friendRequests.decryptAES(key).streamDeserializeToObject[List[BigInt]],
      BigInt(profileID.decryptAES(key)), identityKey, userID)
  }
}

case class F_UserES(userE: F_UserE, //server side F_User which contains some extra authentication info
                   authenticationAnswerHash: BigInt, //the answer to the authentication which was encrypted using the identity key and should come back decrypted to equal this
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

  //name of cooke who contains auth code
  val authenticationCookieName = "authentication"

  def anHourFromNow = {
    val cal = Calendar.getInstance; // creates calendar
    cal.setTime(new Date()); // sets calendar time/date
    cal.add(Calendar.HOUR_OF_DAY, 1); // adds one hour
    cal.getTime
  }
}