package part4

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

class ProbeTestSpec extends TestKit(ActorSystem("ProbeTestSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import ProbeTestSpec._

  "Main actor" should {

    "register a worker" in {
      val mainActor = system.actorOf(Props[MainActor])
      val worker = TestProbe("worker")

      mainActor ! Request("This text contains 18 character", worker.ref)
      expectMsg(RequestAck)
    }

    "send a command to worker" in {
      val mainActor = system.actorOf(Props[MainActor])
      val worker = TestProbe("worker")
      mainActor ! Request("OneWord", worker.ref)
      expectMsg(RequestAck)

      worker.expectMsg(RequestPerWorker("OneWord"))
      worker.reply(Reply(7))

      mainActor ! HowMany
      expectMsg(7)
    }

    "calculate a number of characters in words via worker" in {
      val mainActor = system.actorOf(Props[MainActor])
      val worker = TestProbe("worker")
      mainActor ! Request("This text contains 28 characters", worker.ref)
      expectMsg(RequestAck)

      worker.expectMsg(RequestPerWorker("This"))
      worker.expectMsg(RequestPerWorker("text"))
      worker.expectMsg(RequestPerWorker("contains"))
      worker.expectMsg(RequestPerWorker("28"))
      worker.expectMsg(RequestPerWorker("characters"))

      worker.reply(Reply(4))
      worker.reply(Reply(4))
      worker.reply(Reply(8))
      worker.reply(Reply(2))
      worker.reply(Reply(10))

      mainActor ! HowMany
      expectMsg(28)
    }
  }

}

object ProbeTestSpec {

  case class Request(text: String, workerRef: ActorRef)
  case object RequestAck
  case class RequestPerWorker(word: String)
  case class Reply(charsNumber: Int)
  case object HowMany

  class MainActor extends Actor {

    override def receive: Receive = {
      case Request(text, workerRef) =>
        sender() ! RequestAck

        val words = text.split(" ")
        words.foreach(word => workerRef ! RequestPerWorker(word))

        context.become(accumulateLength(0, 0, words.length))
    }

    def accumulateLength(
                          aggregatedNum: Int,
                          currReply: Int,
                          neededRepliesNumber: Int): Receive = {
      case Reply(num) =>
        if (currReply < neededRepliesNumber - 1)
          context.become(accumulateLength(aggregatedNum + num, currReply + 1, neededRepliesNumber))
        else context.become(replyWithAnswerOnly(aggregatedNum + num))
    }

    def replyWithAnswerOnly(num: Int): Receive = {
      case HowMany => sender() ! num
    }
  }

  class WorkerActor extends Actor {
    override def receive: Receive = {
      case RequestPerWorker(word) => sender() ! Reply(word.length)
    }
  }

}
