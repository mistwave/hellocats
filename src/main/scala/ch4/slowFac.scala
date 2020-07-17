package ch4

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._

object slowFac extends App {
  type Logged[A] = Writer[Vector[String], A]

  def slowly1[A](body: => A) = {
    body
    Thread.sleep(100)
  }

  def slowly[A](body: => A) =
    try body finally Thread.sleep(200)


  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) {
        1.pure[Logged]
      } else {
        slowly(factorial(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans


  }

  //  factorial(5)


  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val Vector((logA, ansA), (logB, ansB)) =
    Await.result(Future.sequence(Vector(
      Future(factorial(3).run),
      Future(factorial(6).run)
    )), 5.seconds)

  List(
    logA, ansA,
    logB, ansB
  ).foreach(println)

}
