package graphnodes

import java.util.Date

case class F_UserProfile(posts: List[BigInt], //list of PostIDs
                       dateOfCreation: Date,
                       albumIDs: List[BigInt],
                       profilePictureID: BigInt,
                       description: String,
                       profileID: BigInt)

object F_UserProfile {
  val profilePictureString = "profilepicture"
  val descriptionString = "description"

  val changableParameters = List(profilePictureString, descriptionString)
}