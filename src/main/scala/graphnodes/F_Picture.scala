package graphnodes

import java.util.Date

case class F_Picture(name: String, description: String,
                      dateOfCreation: Date,
                      fileURI: String,
                      ownerID: BigInt)
