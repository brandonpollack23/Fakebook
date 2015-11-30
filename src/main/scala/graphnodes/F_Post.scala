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
  val locationTypeString = "locationType" //string to show where this is, either in profiles or pages object
    //these are the two options for input to locationTypeString in the Json Doc
    val locationPage = "page"
    val locationProfile = "profile"
  //this is the actual BigInt that is the location
  val locationString = "location"

  val changableParamaters = List(contentsString)
}