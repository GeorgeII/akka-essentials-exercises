package part3

import akka.actor.{Actor, ActorSystem, Props}

object CounterActor extends App {

  class Counter extends Actor {
    import Counter._

    private var counter = 0

    override def receive: Receive = {
      case Increment => counter += 1
      case Decrement => counter -= 1
      case Print     => println(s"Current counter state: $counter")
    }
  }

  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }

  import Counter._

  val actorSystem = ActorSystem("counterSystem")

  val counterActor = actorSystem.actorOf(Props[Counter])

  (0 to 1000).foreach(_ => counterActor ! Increment)
  counterActor ! Print
  (0 to 666).foreach(_ => counterActor ! Decrement)
  counterActor ! Print

  actorSystem.terminate()
}
