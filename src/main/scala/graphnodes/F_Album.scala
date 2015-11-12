package graphnodes

import java.util.Date

case class F_Album(name: String, description: String,
                    dateOfCreation: Date,
                    ownerID: BigInt,
                    images: List[BigInt])
