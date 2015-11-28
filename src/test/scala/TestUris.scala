import java.text.SimpleDateFormat
import java.util.Date

import graphnodes.F_User
import spray.http.Uri
import spray.httpx.RequestBuilding._

trait TestUris {
  val dateFormatter = new SimpleDateFormat("'M'MM'D'dd'Y'yyyy")

  val testDomain = "www.fakebook.com"
  val createUserSuccessRequest = Put(Uri("http://www.fakebook.com/users/newuser") withQuery(F_User.lastNameString -> "Pollack", F_User.firstNameString -> "Brandon", F_User.bioString -> "i like foobar",
    F_User.ageString -> "23", F_User.dobString -> dateFormatter.format(new Date(1992 - 1900, 2, 7))))
}
