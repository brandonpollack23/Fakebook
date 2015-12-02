package graphnodes

import java.util.Date

sealed trait F_PostEOrPost {
  val contents: Any
  val creator: Any
  val locationType: String
  val location: BigInt
  val dateOfCreation: Any
  val postID: BigInt
}

case class F_Post(contents: String,
                   creator: BigInt,
                   locationType: String, //either profile or page
                   location: BigInt,
                   dateOfCreation: Date,
                   postID: BigInt) extends F_PostEOrPost

case class F_PostE(contents: Array[Byte],
                  creator: BigInt,
                  locationType: String, //either profile or page
                  location: Array[Byte],
                  dateOfCreation: Array[Byte],
                  postID: BigInt) extends F_PostEOrPost

object F_Post {
  //these are the queries needed for creation
  val contentsString = "contents"
  val creatorString = "creator"
  val locationTypeString = "locationType" //string to show where this is, either in profiles or pages object
  val locationString = "location"

  //these are the two options for value to locationTypeString in the Json Doc
  val locationPage = "page"
  val locationProfile = "profile"

  val changableParamaters = List(contentsString) //this one doesnt need locationTypeString or locationString because the URI determines those
}