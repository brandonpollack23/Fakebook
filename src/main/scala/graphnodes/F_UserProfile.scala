package graphnodes

import java.util.Date

case class F_UserProfile(posts: List[BigInt], //list of PostIDs
                       dateOfCreation: Date,
                       albumIDs: List[BigInt],
                       profilePictureID: BigInt,
                       description: String,
                       ownerID: BigInt,
                       profileID: BigInt)

case class F_UserProfileE(posts: Array[Byte], //list of PostIDs
                          dateOfCreation: Array[Byte],
                          albumIDs: Array[Byte],
                          profilePictureID: Array[Byte],
                          description: Array[Byte],
                          ownerID: BigInt,
                          profileID: BigInt)

object F_UserProfile {
  val profilePictureString = "profilepicture"
  val descriptionString = "description"

  val changableParameters = List(profilePictureString, descriptionString)
}