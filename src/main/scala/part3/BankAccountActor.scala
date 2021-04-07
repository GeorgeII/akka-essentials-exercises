package part3

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object BankAccountActor extends App {

  object BankAccount {
    case class Deposit(money: Int, replyTo: ActorRef)
    case class Withdraw(money: Int, replyTo: ActorRef)
    case class Statement(message: String, replyTo: ActorRef)
  }

  class BankAccount extends Actor {
    import BankAccount._
    import TransactionStatusPrinter._

    private var money = 0

    override def receive: Receive = {
      case Deposit(amount, replyTo) =>
        money += amount
        replyTo ! Success("Money was successfully deposited!")

      case Withdraw(amount, replyTo) =>
        if (amount > money)
          replyTo ! Failure("Oops, you don't have enough money on your account! Transaction was not" +
            " fulfilled!")
        else {
          money -= amount
          replyTo ! Success("Money was successfully deposited!")
        }

      case Statement("print money", replyTo) =>
        replyTo ! money
    }
  }

  class TransactionStatusPrinter extends Actor {
    import TransactionStatusPrinter._

    override def receive: Receive = {
      case Success(msg) => println(s"I got a reply from the bank: $msg")
      case Failure(msg) => println(s"I got a reply from the bank: $msg")
      case money: Int   => println(s"You have: $$$money")
    }
  }

  object TransactionStatusPrinter {
    case class Success(info: String)
    case class Failure(info: String)
  }

  val actorSystem = ActorSystem("actorSystem")

  val bankAccount = actorSystem.actorOf(Props[BankAccount], "bankAccount")
  val statusPrinter = actorSystem.actorOf(Props[TransactionStatusPrinter], "statusPrinter")

  import BankAccount._

  bankAccount ! Deposit(500, statusPrinter)
  bankAccount ! Statement("print money", statusPrinter)
  bankAccount ! Withdraw(100, statusPrinter)
  bankAccount ! Statement("print money", statusPrinter)

  bankAccount ! Deposit(350, statusPrinter)
  bankAccount ! Statement("print money", statusPrinter)

  bankAccount ! Withdraw(1000, statusPrinter)
  bankAccount ! Statement("print money", statusPrinter)

  actorSystem.terminate()
}
