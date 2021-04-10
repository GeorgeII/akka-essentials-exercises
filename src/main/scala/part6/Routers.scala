package part6

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.routing.{FromConfig, RoundRobinPool}
import com.typesafe.config.ConfigFactory

object Routers extends App {

  class Worker extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(s"I received a message: $message")
    }
  }

  // Creating a pool as usual
//  val system = ActorSystem("actorSystem")
//  val pool = system.actorOf(RoundRobinPool(8).props(Props[Worker]))
//
//  for (i <- 1 to 20) pool ! i
//  system.terminate()

  // Creating a pool via config
  val system = ActorSystem("actorSystem", ConfigFactory.load().getConfig("routerPool"))
  val pool = system.actorOf(FromConfig.props(Props[Worker]), "configurablePool")
  for (i <- 1 to 20) pool ! i
  system.terminate()

}
