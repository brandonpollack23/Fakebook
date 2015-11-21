package graphnodes

import java.util.Date

case class F_User(firstName: String, lastName: String, biography: String, age: Int,
                   dateOfBirth: Date, dateOfCreation: Date,
                   friends: List[BigInt],
                   friendRequests: List[BigInt],
                   profileID: BigInt,
                   ID: BigInt)
