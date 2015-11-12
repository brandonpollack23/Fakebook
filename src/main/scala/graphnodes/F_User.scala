package graphnodes

import java.util.Date

case class F_User(firstName: String, lastName: String, biography: String,
                   dateOfBirth: Date, dateOfCreation: Date,
                   profileID: BigInt,
                   ID: BigInt)
