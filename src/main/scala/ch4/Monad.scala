package ch4

import scala.concurrent.{Await, Future}
import scala.language.higherKinds


trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(a => pure(func(a)))

}

object catsId {

  import cats.Id

  def pure[A](a: A): Id[A] = a

  def map[A, B](ida: Id[A])(f: A => B): Id[B] = f(ida)

  def flatMap[A, B](ida: Id[A])(f: A => Id[B]): Id[B] = f(ida)
}

object app extends App {

  import cats.Monad
  import cats.instances.option._
  import cats.instances.list._
  import cats.instances.future._
  import cats.syntax.applicative._

  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val opt1 = Monad[Option].pure(3)

  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))

  val opt3 = Monad[Option].map(opt2)(_ * 100)

  val list1 = Monad[List].pure(3)

  val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))

  val list3 = Monad[List].map(list2)(a => a + 123)

  val fm = Monad[Future]
  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))


  List(
    opt1, opt2, opt3,
    list1, list2, list3,
    Await.result(future, 1.second),
    1.pure[Option],
    1.pure[List]
  ).foreach(println)


  println("====")

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.Id

  def sumSquare[F[_] : cats.Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))

  def sumSquare2[F[_] : cats.Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  List(
    sumSquare(3.pure[Option], 4.pure[Option]),
    sumSquare2(List(1, 2, 3), List(6, 7)),
    sumSquare(3: Id[Int], 4: Id[Int])
  ).foreach(println)


  import cats.syntax.either._

  val a = 3.asRight[String]

  val b = 4.asRight[String]

}
