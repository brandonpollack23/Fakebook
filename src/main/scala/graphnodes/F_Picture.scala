package graphnodes

import java.util.Date

case class F_Picture(name: String, description: String,
                      dateOfCreation: Date,
                      fileID: BigInt,
                      ownerID: BigInt)

object F_Picture {
  val nameString = "name"
  val descriptionString = "description"
  val ownerString = "owner"

  val changableParameters = List(nameString, descriptionString)
}
