/* TODO
* test listener routes
* test backbone forwarding for each message type
* test each handler handling each type of message
*
* more complex tests
*/

import akka.actor.{ActorSystem, ActorRef, ActorRefFactory}
import akka.event.LoggingAdapter
import akka.testkit.{TestKitBase, ImplicitSender, TestKit}
import graphnodes.F_User
import org.scalatest.{WordSpec, BeforeAndAfterAll, WordSpecLike}
import spray.http.Uri
import system.F_BackBone.CreateUser
import system.{F_ListenerService}

import spray.testkit.ScalatestRouteTest
import spray.routing.HttpService

class FakebookRoutingTests extends WordSpec with ScalatestRouteTest with TestKitBase with ImplicitSender with BeforeAndAfterAll with TestUris with F_ListenerService {

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "The FakeBook route" when {
    "Running route to create a user with PUT" should {
      "forward a message to the backbone to create a new user" in {
        log.info("testing " + createUserSuccessRequest)
        createUserSuccessRequest ~> route ~> check {
          expectMsg(CreateUser(createUserSuccessRequest))
        }
      }
    }
  }

  override implicit def log: LoggingAdapter = akka.event.Logging(system, testActor)

  override def backbone: ActorRef = testActor

  override def actorRefFactory: ActorRefFactory = system
}

class FakebookBackEndTests {
  
}
