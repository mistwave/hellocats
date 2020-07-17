package ch9

import cats.kernel.Monoid
import cats.syntax.traverse._
import cats.instances.future._
import cats.instances.vector._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._


object mr {

  def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B =
    as.map(f).fold(Monoid[B].empty)(Monoid[B].combine)


  def parallelFoldMap[A, B: Monoid](as: Vector[A])(f: A => B): Future[B] = {
    val cpus = Runtime.getRuntime.availableProcessors
    val groupSize = ((1.0 * as.size) / cpus).ceil.toInt
    Future.sequence(
      as.grouped(groupSize)
        .map(chunk => Future {
          foldMap(chunk)(f)
        })
    ).map(bs => bs.fold(Monoid[B].empty)(Monoid[B].combine))
  }
}

object mrTest extends App {

  import mr._
  import cats.Monoid
  import cats.instances.int._
  import cats.instances.string._

  val v = Vector(1, 2, 3)
  List(
    foldMap(v)(identity),
    foldMap(v)(_.toString + "! "),

    parallelFoldMap(v)(identity),
    parallelFoldMap(v)(_.toString + "! "),
  ).foreach(println)

  val result: Future[Int] = parallelFoldMap((1 to 1000000).toVector)(identity)

  println(Await.result(result, 1.second))


}


