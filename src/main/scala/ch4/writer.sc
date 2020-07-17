import cats.data.Writer
import cats.instances.vector._


Writer(Vector(
  "It was the best of times",
  "It was the worst of times"
), 1859)

import cats.syntax.applicative._
import cats.syntax.writer._

type Logged[A] = Writer[Vector[String], A]

123.pure[Logged]

Vector("m1", "m2", "m3").tell

val a = 42.writer(Vector("log for 42", "log2 for 42"))

a.value

a.written

val (log, res) = a.run


val writer1 = for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b


val (log2, res2) = writer1.run

val writer2 = writer1.mapWritten(_.map(_.toUpperCase))

val writer3 = writer2.reset

val writer4 = writer2.swap