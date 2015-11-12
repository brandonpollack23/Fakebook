package graphnodes

import java.util.Date

case class F_UserProfile(posts: List[BigInt], //list of PostIDs
                       dateOfCreation: Date,
                       userList: List[BigInt], //list of userIDs
                       albumIDs: List[BigInt],
                       profilePictureID: BigInt,
                       description: String,
                       ID: BigInt)
