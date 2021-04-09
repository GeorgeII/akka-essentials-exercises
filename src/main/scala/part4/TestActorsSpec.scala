package part4

import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration.DurationInt

class TestActorsSpec extends TestKit(ActorSystem("TestActorsSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import TestActorsSpec._

  "A GreetingActor" should {
    val act = system.actorOf(Props[GreetingActor])

    "respond with 'hello' after 'hi' message" in {
      act ! "hi"

      expectMsg("hello")
    }

    "respond with 'goodbye' after 'bye' message" in {
      act ! "bye"

      expectMsg("goodbye")
    }

    "respond with delay after 'delay' message" in {
      act ! "delay"

      expectMsg(4.second, "delayed")
    }

  }

}

object TestActorsSpec {

  class GreetingActor extends Actor {
    override def receive: Receive = {
      case "hi"    => sender() ! "hello"
      case "bye"   => sender() ! "goodbye"
      case "delay" =>
        Thread.sleep(3500)
        sender() ! "delayed"
    }
  }

}
