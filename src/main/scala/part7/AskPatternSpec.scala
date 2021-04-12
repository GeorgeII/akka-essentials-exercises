package part7

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.pattern.{ask, pipe}
import akka.testkit.{ImplicitSender, TestKit}
import akka.util.Timeout
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.DurationInt

class AskPatternSpec extends TestKit(ActorSystem("SakPatternSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import AskPatternSpec._

  "A cache" should {
    "fail to get value that is not in it" in {
      val cacheHandler = system.actorOf(Props[CacheHandler])
      cacheHandler ! PutInCache("https://google.com", "Page Content")
      cacheHandler ! GetFromCache("http://localhost:8080")
      expectMsg(NotFoundInCache("No such request in cache."))
    }

    "get value previously put into it" in {
      val cacheHandler = system.actorOf(Props[CacheHandler])
      cacheHandler ! PutInCache("http://localhost:8080", "Page Content")
      cacheHandler ! GetFromCache("http://localhost:8080")
      expectMsg(FoundInCache("Value found: Page Content"))
    }
  }
}

object AskPatternSpec {

  case class Put(request: String, response: String)
  case class Get(request: String)

  class CacheActor extends Actor with ActorLogging {
    override def receive: Receive = process(Map())

    def process(cache: Map[String, String]): Receive = {
      case Put(req, res) =>
        log.info(s"I am caching $req - $res")
        context.become(process(cache + (req -> res)))

      case Get(req) =>
        log.info(s"Getting from cache $req")
        sender() ! cache.get(req)
    }
  }


  case class PutInCache(req: String, res: String)
  case class GetFromCache(req: String)
  case class NotFoundInCache(msg: String)
  case class FoundInCache(res: String)

  class CacheHandler extends Actor with ActorLogging {

    implicit val timeout: Timeout = Timeout(1.second)
    implicit val ec: ExecutionContextExecutor = context.dispatcher

    private val cache = context.actorOf(Props[CacheActor])

    override def receive: Receive = {
      case PutInCache(req, res) =>
        cache ! Put(req, res)

      case GetFromCache(req) =>
        val futureResponse = cache ? Get(req)
        val typedFutureResponse = futureResponse.mapTo[Option[String]]
        val transformedFuture = typedFutureResponse.map {
          case None      => NotFoundInCache("No such request in cache.")
          case Some(res) => FoundInCache(s"Value found: $res")
        }

        transformedFuture.pipeTo(sender())
    }
  }
}
