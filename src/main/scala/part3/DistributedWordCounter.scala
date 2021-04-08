package part3

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object DistributedWordCounter extends App {

  object WordCounterMaster {
    case class Initialize(nChildren: Int)
    case class WordCountTask(text: String)
    case class WordCountReply(count: Int)
  }

  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case Initialize(nChildren) =>
        println(s"$self is initializing workers...")
        val childActors = (0 until nChildren).map {
          n => context.actorOf(Props[WordCounterWorker], s"worker-$n")
        }.toVector

        context.become(nextWorkerManager(0, childActors))
    }

    def nextWorkerManager(idx: Int, workers: Vector[ActorRef]): Receive = {
      case WordCountTask(text) =>
        val nextActor = workers(idx)
        nextActor forward WordCountTask(text)

        val nextIdx = if (idx == workers.length - 1) 0 else idx + 1
        context.become(nextWorkerManager(nextIdx, workers))
    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case WordCountTask(text) =>
        val count = text.split(" ").length

        sender() ! WordCountReply(count)
    }
  }


  object Register {
    case class BindMaster(master: ActorRef)
  }

  class Register extends Actor {
    import Register._
    import WordCounterMaster._

    override def receive: Receive = {
      case BindMaster(master) => context.become(processRequestsAndReplies(master))
    }

    def processRequestsAndReplies(master: ActorRef): Receive = {
      case task@WordCountTask(_) => master ! task
      case WordCountReply(count) => println(s"${sender()} just counted the words: $count")
    }
  }


  import WordCounterMaster._
  import Register._

  val system = ActorSystem("actorSys")

  val master = system.actorOf(Props[WordCounterMaster], "master-actor")
  master ! Initialize(10)
  // wait for full initialization
  Thread.sleep(500)

  val register = system.actorOf(Props[Register], "register")
  register ! BindMaster(master)

  register ! WordCountTask("Word")
  register ! WordCountTask("2 words")
  register ! WordCountTask("it is three")
  register ! WordCountTask("this is four words")
  register ! WordCountTask("the string contains five words")
  register ! WordCountTask("I think this one is six")
  register ! WordCountTask("simple")
  register ! WordCountTask("random string")
  register ! WordCountTask("another random string")
  register ! WordCountTask("this consists of four")
  register ! WordCountTask("this one consists of five")
  register ! WordCountTask("length of this text is six")
  register ! WordCountTask("And so on, and so forth")

  system.terminate()

}
