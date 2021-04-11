package part7

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Stash}

object Stashing extends App {

  case object ReadyToProcess
  case class Process(command: String)
  case object NotReadyToProcess

  class StashingActor extends Actor
    with ActorLogging
    with Stash {

    override def receive: Receive = {
      case ReadyToProcess =>
        log.info("I am ready to process your commands!")
        unstashAll()
        context.become(process)

      case Process(cmd) =>
        log.info(s"I am not ready yet. Stashing command [$cmd]...")
        stash()
    }

    def process: Receive = {
      case Process(cmd) =>
        log.info(s"Processing [$cmd]...")

      case NotReadyToProcess =>
        log.info("I'm closing right now!")
        context.become(receive)
    }
  }

  val system = ActorSystem("actorSystem")

  val stashingActor = system.actorOf(Props[StashingActor])

  stashingActor ! Process("The first job for you, actor!")
  stashingActor ! Process("Now get this!")
  stashingActor ! ReadyToProcess
  stashingActor ! Process("This is the 3rd command overall but the 1st one after being opened.")
  stashingActor ! Process("One more command")
  stashingActor ! NotReadyToProcess
  stashingActor ! Process("I guess, this one should end up in the dead letters.")

  system.terminate()

}
