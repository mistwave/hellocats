package ch4

import scala.util.Try

object divide extends App{
  def parseInt(s: String): Option[Int] =
    Try(s.toInt).toOption

  def divide(x: Int, y: Int): Option[Int] =
    if (y == 0) None else Some(x / y)

  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    for {
      x <- parseInt(aStr)
      y <- parseInt(bStr)
      ans <- divide(x, y)
    } yield ans

  List(
    stringDivideBy("6", "3")
      ,stringDivideBy("6", "0")
    ,stringDivideBy("6", "foo")
    ,stringDivideBy("bar", "foo")

  ).foreach(println)
}
