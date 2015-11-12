package graphnodes

import java.io.File
import java.util.Date

case class F_Picture(name: String, description: String,
                      dateOfCreation: Date,
                      file: File,
                      ownerID: BigInt)
