package graphnodes

import java.security.Key
import java.util.Date
import util.Crypto._

case class F_UserProfile(posts: List[BigInt], //list of PostIDs
                       dateOfCreation: Date,
                       defaultAlbum: BigInt,
                       albumIDs: List[BigInt],
                       profilePictureID: BigInt,
                       description: String,
                       ownerID: BigInt,
                       profileID: BigInt) {
  def encryptUserProfile(key: Key) = {
    F_UserProfileE(posts, dateOfCreation, defaultAlbum, albumIDs,
      profilePictureID, description.getBytes.encryptAES(key), ownerID, profileID)
  }
}

case class F_UserProfileE(posts: List[BigInt], //list of PostIDs
                          dateOfCreation: Date,
                          defaultAlbum: BigInt,
                          albumIDs: List[BigInt],
                          profilePictureID: BigInt,
                          description: Array[Byte],
                          ownerID: BigInt,
                          profileID: BigInt) {
  def decryptUserProfileE(key: Key) = {
    F_UserProfile(posts, dateOfCreation, defaultAlbum,
      albumIDs, profilePictureID, description.decryptAES(key).byteArray2String,
      ownerID, profileID)
  }
}

object F_UserProfile {
  //changable parameters
  val profilePictureIDField = "profilePictureID"
  val descriptionField = "description"
}