package effect

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._
import scala.concurrent._
import scala.concurrent.duration._


class Intro {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val CS: ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO] = IO.timer(global)

  def yolo[A](desc: IO[A]): A = desc.unsafeRunSync()

  val ios: IO[Unit] = IO {
    println("hellllllllo!")
  }

  IOApp

}
