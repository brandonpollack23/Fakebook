package graphnodes

import java.util.Date

case class F_Page(name: String, description: String,
                   dateOfCreation: Date,
                   userList: List[BigInt], //list of userID
                   posts: List[BigInt],
                   albumIDs: List[BigInt],
                   pictureID: BigInt,
                   ownerID: BigInt)

object F_Page {
  val nameString = "name"
  val descriptionString = "description"
  val ownerString = "owner"

  val changableParameters = List(nameString, descriptionString, ownerString)
}
