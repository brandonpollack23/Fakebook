package graphnodes

import java.util.Date

case class F_Page(name: String, description: String,
                   dateOfCreation: Date,
                   userList: List[BigInt], //list of userID
                   posts: List[BigInt],
                   defaultAlbumID: BigInt,
                   albumIDs: List[BigInt],
                   pictureID: BigInt,
                   ownerID: BigInt,
                   ID: BigInt)

object F_Page {
  //query that contains userID of who to add to page
  val newUserString = "newuser"

  //changable fields
  val nameField = "name"
  val descriptionField = "description"
}
