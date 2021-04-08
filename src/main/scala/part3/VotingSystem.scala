package part3

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object VotingSystem extends App {

  case class Vote(candidate: String)
  case object VoteStatusRequest
  case class VoteStatusReply(candidate: Option[String])

  class Citizen extends Actor {

    override def receive: Receive = handleRequest(None)

    def handleRequest(candidate: Option[String]): Receive = {
      case Vote(candidate)   =>
        println(s"${context.self} voted.")
        context.become(handleRequest(Option(candidate)))
      case VoteStatusRequest =>
        sender() ! VoteStatusReply(candidate)
    }

  }

  case class AggregateVotes(citizens: Set[ActorRef])

  class VoteAggregator extends Actor {

    override def receive: Receive = {
      case AggregateVotes(citizens) =>
        citizens.foreach(citizen => citizen ! VoteStatusRequest)
        println("Aggregating...")
        context.become(gatherReplies(citizens, Map.empty))
    }

    def gatherReplies(citizensNotReplied: Set[ActorRef], votes: Map[String, Int]): Receive = {
      case VoteStatusReply(Some(candidate)) =>
        println(s"Not gathered: $citizensNotReplied, replyFrom: ${sender()}")
        val updatedVotes = votes.get(candidate) match {
          case Some(value) => votes + (candidate -> (value + 1))
          case None        => votes + (candidate -> 1)
        }

        val citizensLeft = citizensNotReplied - sender()

        if (citizensLeft.nonEmpty)
          context.become(gatherReplies(citizensLeft, updatedVotes))
        else {
          println(updatedVotes)
          context.become(receive)
        }
    }
  }

  val system = ActorSystem("system")

  val alice = system.actorOf(Props[Citizen])
  val bob = system.actorOf(Props[Citizen])
  val charlie = system.actorOf(Props[Citizen])
  val daniel = system.actorOf(Props[Citizen])

  alice   ! Vote("Martin")
  bob     ! Vote("Jonas")
  charlie ! Vote("Roland")
  daniel  ! Vote("Roland")

  val voteAggregator = system.actorOf(Props[VoteAggregator])
  voteAggregator ! AggregateVotes(Set(alice, bob, charlie, daniel))

}
