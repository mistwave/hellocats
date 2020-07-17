package ch1

import cats._
import cats.implicits._

object EqTest extends App {

  123 === 123

  (Some(1): Option[Int]) === None

  1.some === none[Int]

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] {
    (x, y) =>
      (x.name === y.name) &&
        (x.age === y.age) &&
        (x.color === y.color)
  }
  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 === cat2)
  println(cat2 === cat2)
  println(optionCat1 === optionCat1)
  println(optionCat2 === optionCat1)

}
