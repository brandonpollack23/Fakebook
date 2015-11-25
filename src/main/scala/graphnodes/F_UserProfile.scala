package graphnodes

import java.util.Date

case class F_UserProfile(posts: List[BigInt], //list of PostIDs
                       dateOfCreation: Date,
                       albumIDs: List[BigInt],
                       profilePictureID: BigInt,
                       description: String,
                       ID: BigInt)
