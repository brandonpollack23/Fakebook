package graphnodes

import java.security.{PrivateKey, Key, PublicKey}
import java.util.{Calendar, Date}
import javax.crypto.SecretKey
import javax.crypto.spec.SecretKeySpec
import util.Crypto._
import F_User._
case class F_User(firstName: String, lastName: String, biography: String, age: Int,
                   dateOfBirth: Date, dateOfCreation: Date,
                   friends: List[(BigInt, SecretKey)], //each friend is now tupled with their friend secret key encrypted with your own identity private key, so this key is user's RSA_pu(friendAES)
                   friendRequests: List[(BigInt, Array[Byte])],
                   profileID: BigInt,
                   identityKey: PublicKey, //this is the general public key used for authenticaiton for this user, as wella s friend requesting, options are because this isnt on user side
                   userID: BigInt) {
  def encryptUser(key: Key) = {
    F_UserE(firstName.getBytes.encryptAES(key), lastName.getBytes.encryptAES(key), biography.getBytes.encryptAES(key), age.toByteArray.encryptAES(key),
      dateOfBirth.toByteArray.encryptAES(key), dateOfCreation, friends.map(x => (x._1, x._2.toByteArray.encryptRSA(identityKey))), friendRequests,
      profileID, identityKey, userID)
  }
}

case class F_UserE(firstName: Array[Byte], lastName: Array[Byte], biography: Array[Byte], age: Array[Byte],
                   dateOfBirth: Array[Byte], dateOfCreation: Date,
                   friends: List[(BigInt, Array[Byte])], //can map out friend graph, but better than proviing key when deleting, need this to unfriend everyone
                   friendRequests: List[(BigInt, Array[Byte])],
                   profileID: BigInt,
                   identityKey: PublicKey, //this is the general public key used for authenticaiton for this user, as wella s friend requesting, options are because this isnt on user side
                   userID: BigInt) {
  def decryptUserE(key: Key, privateKey: PrivateKey) = {
    F_User(firstName.decryptAES(key).byteArray2String, lastName.decryptAES(key).byteArray2String, biography.decryptAES(key).byteArray2String, age.decryptAES(key).byteArray2Int,
      dateOfBirth.decryptAES(key).streamDeserializeToObject[Date], dateOfCreation,
      friends.map(x => (x._1, createSecretKeyFromBytes(x._2.decryptRSA(privateKey)))), friendRequests,
      profileID, identityKey, userID)
  }
}

case class F_UserES(userE: F_UserE, //server side F_User which contains some extra authentication info
                   authenticationAnswerHash: BigInt, //the answer to the authentication which was encrypted using the identity key and should come back decrypted to equal this
                   sessionExpiration: Date = new Date) //the latest that the sessionAEScookie will work before authentication failed gets sent back, default is one hour later

object F_User {
  val friendRequestString = "requestfriend" //QUERY used to say who the request is for: BigInt String
  val acceptFriendString = "acceptfriend"   //QUERY used to say accept friend: true or false string
  val friendRemoveString = "removefriend"  //This query is the only one needed when doing the uri for removing friends /users/remove/REMOVERID: BigInt String

  //name of cooke who contains auth code
  val authenticationCookieName = "authentication"

  //fields for updating server side
  val lastNameField = "lastName"
  val firstNameField = "firstName"
  val bioField = "biography"
  val ageField = "age"
  val dobField = "dateOfBirth"

  //query to specify who the owner is when authenticating
  val ownerQuery = "owner"

  def anHourFromNow = {
    val cal = Calendar.getInstance; // creates calendar
    cal.setTime(new Date()); // sets calendar time/date
    cal.add(Calendar.HOUR_OF_DAY, 1); // adds one hour
    cal.getTime
  }

  def createSecretKeyFromBytes(in: Array[Byte]): SecretKey = {
    new SecretKeySpec(in, 0, in.length, "AES")
  }
}