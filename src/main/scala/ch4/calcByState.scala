package ch4

import cats.data.State

object calcByState extends App {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }


  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ =>
        sys.error("Fail!")
    }


  List(
    evalOne("42").runA(Nil).value,
    evalOne("22").run(Nil).value,
    "==="
  ) foreach println


  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  import cats.syntax.applicative._

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) {(acc, token) =>
      acc.flatMap(_ => evalOne(token))
    }


  List(
    program.runA(Nil).value,
    evalAll("1 2 + 3 *".split(" ").toList).runA(Nil).value
  ) foreach println


  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value

  println(evalInput("1 2 + 3 4 + *"))

}
