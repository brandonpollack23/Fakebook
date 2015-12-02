package graphnodes

import java.util.Date

case class F_Picture(name: String, description: String,
                      containingAlbum: BigInt,
                      dateOfCreation: Date,
                      fileID: BigInt,
                      pictureID: BigInt,
                      ownerID: BigInt)

case class F_PictureE(name: Array[Byte], description: Array[Byte],
                     containingAlbum: Array[Byte],
                     dateOfCreation: Array[Byte],
                     fileID: Array[Byte],
                     pictureID: BigInt,
                     ownerID: BigInt)

object F_Picture {
  //in creation query string
  val nameString = "name"
  val descriptionString = "description"
  val ownerString = "owner"
  val albumString = "album" //albumID to move to is the value of this query

  val changableParameters = List(nameString, descriptionString, albumString)
}
