package graphnodes

import java.util.Date

case class F_Post(contents: String,
                   creator: BigInt,
                   locationType: String, //either profile or page
                   location: BigInt,
                   dateOfCreation: Date,
                   postID: BigInt)

object F_Post {
  val contentsString = "contents"
  val creatorString = "creator"
  val locationType = "location"
  val locationTypeString = "locationType"
    val locationPage = "page"
    val locationProfile = "profile"
  val locationString = "location"

  val changableParamaters = List(contentsString)
}