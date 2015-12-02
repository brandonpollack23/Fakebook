package graphnodes

import java.util.Date

case class F_Page(name: String, description: String,
                   dateOfCreation: Date,
                   userList: List[BigInt], //list of userID
                   posts: List[BigInt],
                   albumIDs: List[BigInt],
                   pictureID: BigInt,
                   ownerID: BigInt,
                   ID: BigInt)

object F_Page {
  //in creation query string
  val joinPageString = "join"
  val leavePageString = "leave"
  val newUserString = "newuser"
  val nameString = "name"
  val descriptionString = "description"
  val ownerString = "owner"

  val changableParameters = List(nameString, descriptionString, ownerString, joinPageString, leavePageString)
}
