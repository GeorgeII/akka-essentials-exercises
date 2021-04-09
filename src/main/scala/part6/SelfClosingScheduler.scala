package part6

import akka.actor.{Actor, ActorLogging, ActorSystem, Cancellable, Props}

import scala.concurrent.duration.DurationInt

object SelfClosingScheduler extends App {

  class SelfClosingActor extends Actor with ActorLogging {
    import context.dispatcher

    override def receive: Receive = {
      case _ =>
        val scheduler = context.system.scheduler.scheduleOnce(1.second) { context.stop(self) }
        context.become(waitToExpire(scheduler))
    }

    def waitToExpire(scheduler: Cancellable): Receive = {
      case _ =>
        scheduler.cancel()
        log.info("The timer has been restarted.")

        val newScheduler = context.system.scheduler.scheduleOnce(1.second) {
          log.info("Actor is stopping...")
          context.stop(self)
        }
        context.become(waitToExpire(newScheduler))
    }
  }


  val system = ActorSystem("ActorSystem")
  val selfClosingActor = system.actorOf(Props[SelfClosingActor])

  selfClosingActor ! "a"
  Thread.sleep(300)
  selfClosingActor ! "2"
  Thread.sleep(500)
  selfClosingActor ! "c"
  Thread.sleep(700)
  selfClosingActor ! "4"

  Thread.sleep(1200)
  selfClosingActor ! "should be stopped now"

  system.terminate()

}
