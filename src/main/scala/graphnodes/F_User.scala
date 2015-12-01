package graphnodes

import java.util.Date

case class F_User(firstName: String, lastName: String, biography: String, age: Int,
                   dateOfBirth: Date, dateOfCreation: Date,
                   friends: List[BigInt],
                   friendRequests: List[BigInt],
                   profileID: BigInt,
                   userID: BigInt)

object F_User {
  val lastNameString = "lastname"
  val firstNameString = "firstname"
  val bioString = "bio"
  val ageString = "age"
  val dobString = "dob"

  val changableParameters = List(lastNameString, firstNameString, bioString, ageString, dobString, friendRequestString, acceptFriendString)

  val friendRequestString = "friendrequest"
  val acceptFriendString = "acceptfriend"
  val friendRemoveString = "remove"
}
