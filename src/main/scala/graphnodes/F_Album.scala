package graphnodes

import java.util.Date

case class F_Album(name: String, description: String,
                    dateOfCreation: Date,
                    ownerID: BigInt,
                    id: BigInt,
                    images: List[BigInt])

object F_Album {
  val nameString = "name"
  val descriptionString = "description"
  val ownerString = "owner"

  val changableParameters = List(nameString, descriptionString)
}