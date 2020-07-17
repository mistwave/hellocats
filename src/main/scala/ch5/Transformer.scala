package ch5

import cats.data.EitherT

object Transformer extends App {
  import cats.data.OptionT
  import cats.Monad
  import cats.instances.list._
  import cats.syntax.applicative._


  type ListOption[A] = OptionT[List, A]

  val result1 : ListOption[Int] = OptionT(List(Option(10)))
  println(result1)

  val result2 : ListOption[Int] = 32.pure[ListOption]
  println(result2)

  val r3 = for {
    x <- result1
    y <- result2
  } yield x + y

  println(r3)

  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]

  val c = a.flatMap(x => b.map(y => x + y))

  import scala.concurrent.Future
  import cats.data.{EitherT, OptionT}
  type FutureEither[A] = EitherT[Future,String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]


  import cats.instances.future._
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b
}
