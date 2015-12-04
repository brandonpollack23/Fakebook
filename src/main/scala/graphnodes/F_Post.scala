package graphnodes

import java.security.Key
import java.util.Date
import util.Crypto._

sealed trait F_PostEOrPost {
  val contents: Any
  val creator: BigInt
  val locationType: String //either profile or page
  val location: BigInt
  val dateOfCreation: Date
  val postID: BigInt
}

case class F_Post(contents: String,
                   creator: BigInt,
                   locationType: String, //either profile or page
                   location: BigInt,
                   dateOfCreation: Date,
                   postID: BigInt) extends F_PostEOrPost{
  def encryptPost(key: Key) = {
    F_PostE(contents.getBytes.encryptAES(key), creator, locationType, location, dateOfCreation,
      postID)
  }
}

case class F_PostE(contents: Array[Byte],
                  creator: BigInt,
                  locationType: String, //either profile or page
                  location: BigInt,
                  dateOfCreation: Date,
                  postID: BigInt) extends F_PostEOrPost{
  def decryptPost(key: Key) = {
    F_Post(contents.decryptAES(key).byteArray2String, creator, locationType, location,
      dateOfCreation, postID)
  }
}

object F_Post {
  //this is the one query
  val locationTypeString = "locationType" //string to show where this is, either in profiles or pages object

  //these are the two options for value to locationTypeString in the Json Doc and query
  val locationPage = "page"
  val locationProfile = "profile"

  //changable parameter for use on server
  val contentsField = "contents"
}