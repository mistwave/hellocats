
import cats.data.State

val a = State[Int, String] { state =>
  (state, s"The state is $state")
}

val (s, res) = a.run(10).value


val state = a.runS(10).value

val result = a.runA(10).value


val step1 = State[Int, String] {num =>
  val ans = num + 1
  (ans, s"Result of step1: $ans")
}

val step2 = State[Int, String] { num =>
  val ans = num * 2
  (ans, s"Result of step2: $ans")
}

val both = for {
  a <- step1
  b <- step2
} yield (a, b)

val (ss, rr) = both.run(20).value

import State._

val program: State[Int, (Int, Int, Int)] = for {
  a <- get[Int]
  _ <- set[Int](a + 1)
  b <- get[Int]
  _ <- modify[Int](_ + 1)
  c <- inspect[Int, Int](_ * 1000)
} yield (a, b, c)

val (ps, pr) = program.run(1).value