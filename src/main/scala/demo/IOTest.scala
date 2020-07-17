package demo

import cats.effect.IO

import scala.io.StdIn


object IOTest extends App{

  def putStrLn(line: String): IO[Unit] =
    IO {
      println(line)
    }

  val getStrLn: IO[String] =
    IO {
      StdIn.readLine
    }

  val program: IO[Unit] = for {
    _ <- putStrLn("Welcome to cats IO! What's your name?")
    name <- getStrLn
    _ <- putStrLn(s"Well hello, $name!")
  } yield ()

  program.unsafeRunSync()

}
