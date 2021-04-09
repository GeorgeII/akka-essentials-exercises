package part5

import akka.actor.SupervisorStrategy.{Escalate, Restart, Resume, Stop}
import akka.actor.{Actor, ActorRef, ActorSystem,
                    OneForOneStrategy, Props, SupervisorStrategy, Terminated}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.concurrent.duration.DurationInt

class SupervisionSpec extends TestKit(ActorSystem("SupervisionSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll{

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import SupervisionSpec._

  "Supervisor" should {
    "create a child actor with given props" in {
      val supervisor = system.actorOf(Props[Supervisor])
      val childProps = Props[ChildActorToFail]
      supervisor ! childProps
      expectMsgType[ActorRef](1.seconds)
    }

    "stop a child when the child throws NPE" in {
      val supervisor = system.actorOf(Props[Supervisor])
      val childProps = Props[ChildActorToFail]
      supervisor ! childProps

      val child = expectMsgType[ActorRef]

      watch(child)

      child ! "stop"

      val actorStopMessage = expectMsgType[Terminated]
      assert(actorStopMessage.actor == child)
    }

    "restart a child after the child throws RuntimeException" in {
      val supervisor = system.actorOf(Props[Supervisor])
      val childProps = Props[ChildActorToFail]
      supervisor ! childProps

      val child = expectMsgType[ActorRef]

      child ! Increment
      child ! Increment
      child ! RevealSum
      expectMsg(2)

      child ! "restart"

      child ! RevealSum
      expectMsg(0)
    }

    "resume a child after the child throws IllegalArgumentException" in {
      val supervisor = system.actorOf(Props[Supervisor])
      val childProps = Props[ChildActorToFail]
      supervisor ! childProps

      val child = expectMsgType[ActorRef]

      child ! Increment
      child ! Increment
      child ! RevealSum
      expectMsg(2)

      child ! "resume"

      child ! RevealSum
      expectMsg(2)
    }

    "escalate when the child throws Exception" in {
      val supervisor = system.actorOf(Props[Supervisor])
      val childProps = Props[ChildActorToFail]
      supervisor ! childProps

      val child = expectMsgType[ActorRef]

      watch(child)

      child ! Increment
      child ! Increment
      child ! RevealSum
      expectMsg(2)

      child ! "escalate"

      val escalationMsg = expectMsgType[Terminated]
      assert(escalationMsg.actor == child)
    }
  }

  "Child" should {
    "be able to update its state and reveal it" in {
      val supervisor = system.actorOf(Props[Supervisor])
      val childProps = Props[ChildActorToFail]
      supervisor ! childProps

      val child = expectMsgType[ActorRef]

      watch(child)

      child ! Increment
      child ! Increment
      child ! Increment
      child ! RevealSum
      expectMsg(3)
    }
  }
}

object SupervisionSpec {

  class Supervisor extends Actor {
    override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
      case _: NullPointerException     => Stop
      case _: IllegalArgumentException => Resume
      case _: RuntimeException         => Restart
      case _: Exception                => Escalate
    }

    override def receive: Receive = {
      case props: Props =>
        val childActor = context.actorOf(props)

        sender() ! childActor
    }
  }


  case object Increment
  case object RevealSum

  class ChildActorToFail extends Actor {
    var sum = 0

    override def receive: Receive = {
      case Increment  => sum += 1
      case RevealSum  => sender() ! sum
      case "stop"     => throw new NullPointerException("You passed the stop word.")
      case "restart"  => throw new RuntimeException("You passed the restart word.")
      case "resume"   => throw new IllegalArgumentException("You passed the resume word.")
      case "escalate" => throw new Exception("You passed the escalate word.")
    }
  }
}
