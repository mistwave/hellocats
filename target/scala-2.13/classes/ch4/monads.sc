
import cats.syntax.either._

val a = 3.asRight[String]

val b = 4.asRight[String]

for {
  x <- a
  y <- b
} yield x * x + y * y

def countPositive(nums: List[Int]) =
  nums.foldLeft(0.asRight[String]) { (acc, num) =>
    if (num > 0) acc.map(_ + 1)
    else Left("Negative. Stopping!")
  }

countPositive(List(1, 2, 3))
countPositive(List(1, 2, 3, -1, 1))

"Error".asLeft[Int].getOrElse(0)

"Error".asLeft[Int].orElse(2.asRight[String])

(-1).asRight[String].ensure("Must be non-negative!")(_ > 0)

for {
  a <- 1.asRight[String]
  b <- 0.asRight[String]
  c <- if (b == 0) "DIV0".asLeft[Int] else (a / b).asRight[String]
} yield c * 100


import cats.Eval

val now = Eval.now(math.random + 1000)
val later = Eval.later(math.random + 2000)
val always = Eval.always(math.random + 3000)

now.value
now.value

later.value
later.value

always.value
always.value

val saying = Eval.
  always {
    println("Step 1"); "The cat"
  }.
  map { str => println("Step 2"); s"$str sat on" }.
  memoize.
  map { str => println("Step 3"); s"$str the mat" }

saying.value





saying.value

def fac(n: Int): Eval[BigInt] =
  if (n == 1) Eval.now(n)
  else Eval.defer(fac(n - 1).map(_ * n))


fac(50000).value

import ch4.evals

evals.foldRight((1 to 100000).toList, 0)(_ + _)