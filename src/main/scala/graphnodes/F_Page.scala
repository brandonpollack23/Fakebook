package graphnodes

import java.util.Date

case class F_Page(name: String, description: String,
                   dateOfCreation: Date,
                   userList: List[BigInt], //list of userID
                   posts: List[BigInt],
                   albumIDs: List[BigInt],
                   pictureID: BigInt,
                   ownerID: BigInt)
