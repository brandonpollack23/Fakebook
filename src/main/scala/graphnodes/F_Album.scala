package graphnodes

import java.util.Date

case class F_Album(name: String, description: String,
                    dateOfCreation: Date,
                    isDefault: Boolean,
                    ownerID: BigInt,
                    id: BigInt,
                    images: List[BigInt])

case class F_AlbumE(name: Array[Byte], description: Array[Byte],
                   dateOfCreation: Array[Byte],
                   isDefault: Array[Byte],
                   ownerID: BigInt,
                   id: BigInt,
                   images: Array[Byte])

object F_Album {
  val nameString = "name"
  val descriptionString = "description"
  val ownerString = "owner"

  val changableParameters = List(nameString, descriptionString)
}