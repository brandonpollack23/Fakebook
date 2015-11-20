import akka.testkit.TestActorRef
import org.scalatest.{WordSpec}
import spray.http.HttpHeaders
import spray.testkit.ScalatestRouteTest
import system.F_Listener

class FakebookRoutingTests extends WordSpec with ScalatestRouteTest {
  val route = TestActorRef(new F_Listener(null)).underlyingActor.route
  //implicit val host = DefaultHostInfo(HttpHeaders.Host("fake.fakebook.com"), false)

  "the route" should {

    "succeed on requesting user info if it exists" in {
      Get("/user/1234") ~> route ~> check {

      }
    }

    "succeed on updating user info if they exist" in {
      Post("/user/5678") ~> route ~> check{

      }
    }

    "succeed on path to create a user" in {
      Put("/user/newuser") ~> route ~> check {

      }
    }
  }
}
