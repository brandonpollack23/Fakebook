/* TODO
* test listener routes
* test backbone forwarding for each message type
* test each handler handling each type of message
*
* more complex tests
*/

import java.net.{Inet4Address, InetSocketAddress}

import akka.actor.Status.Success
import akka.actor.{ActorRef, ActorRefFactory, ActorSystem}
import akka.event.LoggingAdapter
import akka.testkit.{TestActorRef, ImplicitSender, TestKit, TestKitBase}
import akka.pattern.ask
import akka.util.Timeout
import org.scalatest.{WordSpecLike, BeforeAndAfterAll, WordSpec}
import spray.can.Http
import spray.http.{HttpResponse, HttpMethods, HttpRequest, HttpHeaders}
import spray.json.JsArray
import spray.testkit.ScalatestRouteTest
import system.F_BackBone.CreateUser
import system.workers.{F_PageProfileHandler, F_UserHandler, F_PictureHandler}
import system.{F_BackBone, F_Listener, F_ListenerService, F_Server}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

class FakebookRoutingTests extends WordSpec with ScalatestRouteTest with TestKitBase with ImplicitSender with BeforeAndAfterAll with TestUris with F_ListenerService {

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
  }

  implicit val defaultHostInfo = DefaultHostInfo(HttpHeaders.Host("www.fakebook.com"), securedConnection = false)

  val testRoute = {
    pathPrefix("test") {
      pathEnd {
        complete("worked")
      }
    }
  }

  "Basic Route tests" when {

    "Initial basic routing test" should {
        "respond with worked" in {
          Put("/test") ~> testRoute ~> check {
            assert(responseAs[String] == "worked")
          }
        }
    }
  }

  "The FakeBook route" when {

    "When sending a single slash request" should {
      "respond with a simple pong" in {
        Get() ~> logRequestResponse("plain get final request and response")(sealRoute(route)) ~> check {
          assert(responseAs[String] == "pong")
        }
      }
    }

    "Running route to create a user with PUT" should {
      "forward a message to the backbone to create a new user" in {
        createUserSuccessRequest ~> logRequest("create user final request and response"){sealRoute(route)} ~> check {
          expectMsg(CreateUser(createUserSuccessRequest))
        }
      }
    }
  }

  override implicit def log: LoggingAdapter = akka.event.Logging(system, testActor)

  override def backbone: ActorRef = testActor

  override def actorRefFactory: ActorRefFactory = system
}

class FakebookBackEndTests(_system: ActorSystem) extends TestKit(_system) with WordSpecLike with ImplicitSender with BeforeAndAfterAll with TestUris  {

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
  }

  import system.dispatcher

  implicit val timeout = Timeout(5 seconds)

  val server: ActorRef = TestActorRef(new F_Server)

  "The F_Server actor" when {

    "Any connection comes in" should {
      "spawn a F_Listener child actor to handle the requests" in {
        server ! Http.Connected(new InetSocketAddress(0), new InetSocketAddress(0))
        expectMsgClass(classOf[Http.Register])
      }
    }
  }

  val backbone: ActorRef = TestActorRef(new F_BackBone(TestActorRef(new F_PictureHandler(backbone)), TestActorRef(new F_UserHandler(backbone)), TestActorRef(new F_PageProfileHandler(backbone))))
  val testListener = TestActorRef(new F_Listener(backbone, testActor))
  
  "The F_Listener actor" when {
    "An HttpRequest to create a user comes in" should {
      "return a new user JSON" in {
        (testListener ? createUserSuccessRequest).mapTo[HttpResponse].map(_.entity)
        expectMsgClass(classOf[JsArray])
      }
    }
  }
}
