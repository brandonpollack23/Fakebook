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
  //profiles are automatically created, you only use POSTs to update them after the fact (they are created when a newuser is)
  val profilePictureString = "profilepicture"
  val descriptionString = "description"

  val changableParameters = List(profilePictureString, descriptionString)
}