package part3

import akka.actor.{Actor, ActorSystem, Props}

object CounterActorViaBecome extends App {

  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }

  class Counter extends Actor {
    import Counter._

    override def receive: Receive = count(0)

    def count(counter: Int): Receive = {
      case Increment => context.become(count(counter + 1))
      case Decrement => context.become(count(counter - 1))
      case Print     => println(s"Current state: $counter")
    }
  }

  import Counter._

  val actorSystem = ActorSystem("actorSystem")

  val counter = actorSystem.actorOf(Props[Counter])

  counter ! Increment
  counter ! Print
  counter ! Decrement
  counter ! Print

  (0 to 9).foreach(_ => counter ! Increment)
  counter ! Print
  (0 to 99).foreach(_ => counter ! Decrement)
  counter ! Print

  actorSystem.terminate()

}
