import akka.typed._
import akka.typed.scaladsl.Actor
import akka.typed.scaladsl.AskPattern._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.Await
import akka.util.Timeout
import akka.actor.Scheduler
import scala.concurrent.ExecutionContext


/**
  * BankAcountActors contains the business logic for the bank, using typed actors.
  ****/
object BankAcountActors{


  /**
    * The set of primitive instructions the actor understands. In this simple
    * example, each primitive operation mirrors exactly one operation in the
    * Bank interface.
    ****/
  sealed trait Command
  object Command {
    case object CloseAccount                          extends Command

    case class  GetBalance(
                  replyTo: ActorRef[Option[Int]])     extends Command

    case class  IncrementBalance(
                  increment: Int,
                  replyTo: ActorRef[Option[Int]])     extends Command
  }

  import Command._

  /**
    * Given the current balance (which is, in practice, the state in a functional
    * way), computes an actor capable of a) processing the next message and
    * b) returning another actor which is going to do the same, in turn.
    ****/
  private def nextBankActor(balance: Option[Int]): Behavior[Command] =
    Actor.immutable[Command] {

      case (_, CloseAccount) =>
        nextBankActor(None)

      case (_, IncrementBalance(i, replyTo)) =>
        val r = balance.map(i.+)
        replyTo ! r
        nextBankActor(r)

      case (_, GetBalance(replyTo)) =>
        replyTo ! balance
        Actor.same

    }

  /**
    * The actor which is going to process the commands received from the
    * `AkkaTypedBankAccount`
    ****/
  val initialBankActor = nextBankActor(Some(0))
}

/**
  * All the environmental things needed by the `AkkaTypedBankAccount`.
  * Mainly:
  *   - The ActorSystem (and some derivatives)
  *   - Configuration settings (currently, only the timeout)
  ****/
case class BankAcountEnvironment() {

  import BankAcountActors._

  val system: ActorSystem[Command] =
    ActorSystem(initialBankActor, "bankActorSystem")

  implicit val executionContext: ExecutionContext =
    system.executionContext

  implicit val scheduler: Scheduler =
    system.scheduler

  implicit val timeout: Timeout =
    Timeout(5.minutes)
}

/**
  * Some helper utilities
  ****/
object Extensions {

  /**
    * A synchronous implementation of the AskPattern. I would never implement
    * an await for production code, but the BankAccount has a synchronous
    * interface which makes the waiting mandatory. For this reason I've implemented
    * a nicer api for it.
    ****/
  implicit class SyncAskable[T](val ref: ActorRef[T]) extends AnyVal {
    def syncAsk[U](f: (ActorRef[U]) => T)(implicit timeout: Timeout, scheduler: Scheduler): U = {
      val r =
        ref ? f

      Await.result(r, timeout.duration)
    }
  }

}


case class AkkaTypedBankAccount() extends BankAccount {

  val environment = BankAcountEnvironment()

  import BankAcountActors._
  import environment._
  import Extensions._


  def closeAccount(): Unit =
      system ! Command.CloseAccount


  def getBalance: Option[Int] =
      system   syncAsk   (Command.GetBalance(_))


  def incrementBalance(increment: Int): Option[Int] =
      system   syncAsk   (Command.IncrementBalance(increment, _: ActorRef[Option[Int]]))

}
