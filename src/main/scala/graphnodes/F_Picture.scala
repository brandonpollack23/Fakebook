package graphnodes

import java.util.Date

case class F_Picture(name: String, description: String,
                      containingAlbum: BigInt,
                      dateOfCreation: Date,
                      fileID: BigInt,
                      pictureID: BigInt,
                      ownerID: BigInt)

object F_Picture {
  val nameString = "name"
  val descriptionString = "description"
  val ownerString = "owner"
  val albumString = "album"

  val changableParameters = List(nameString, descriptionString, albumString)
}
