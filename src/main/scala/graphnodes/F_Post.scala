package graphnodes

import java.util.Date

case class F_Post(contents: String,
                   creator: BigInt,
                   locationType: String, //either profile or page
                   location: BigInt,
                   dateOfCreation: Date)