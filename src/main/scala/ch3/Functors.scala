package ch3

import cats.Functor

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.instances.function._
import cats.syntax.functor._

import scala.language.higherKinds

object Functors extends App {
  val future: Future[String] =
    Future(0)
      .map(_ + 10)
      .map(_ * 4)
      .map(_ + 2)
      .map("The Ultimate Answer to Life, The Universe and Everything is..." + _ + "!")

  println(Await.result(future, 1.second))

  val func1: Int => String =
    (x: Int) => x.toString

  val func2: String => String =
    (y: String) => y + y

  println((func1 map func2) (2))
  println((func1 andThen func2) (2))
  println(func2(func1(2)))

  val func =
    ((x: Int) => x.toDouble)
      .map(x => x + 1)
      .map(x => x * 2).
      map(x => x + "!")

  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](as: List[A])(f: A => B): List[B] = as.map(f)

    identity()
  }
}


