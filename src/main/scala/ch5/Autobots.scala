package ch5

import cats.data.EitherT
import cats.syntax.applicative._
import cats.instances.future._
import cats.syntax.flatMap._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Autobots extends App {

  val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

  //    type Response[A] = Future[Either[String, A]A]
  type Response[A] = EitherT[Future, String, A]

  //
  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(x) => EitherT.right(Future(x))
      case None => EitherT.left(Future(s"$autobot unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      a <- getPowerLevel(ally1)
      b <- getPowerLevel(ally2)
      sum = a + b
    } yield sum > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value

    Await.result(stack, 1.second) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
    }
  }


  println(tacticalReport("Jazz", "Bumblebee")) // res28: String = Jazz and Bumblebee need a recharge.

  println(tacticalReport("Bumblebee", "Hot Rod")) // res29: String = Bumblebee and Hot Rod are ready to roll out!

  println(tacticalReport("Jazz", "Ironhide")) // res30: String = Comms error: Ironhide unreachable

  //
  //  println(getPowerLevel("Jazz"))
  //
  //  val buzz = getPowerLevel("BUzz")
  //  val sm1 = canSpecialMove("Hot Rod", "Jazz")
  //  val sm2 = canSpecialMove("Bumblebee", "Jazz")
  //  Thread.sleep(1000)
  //
  //
  //  println(buzz)
  //  println(sm1)
  //  println(sm2)


}
