package ch1

;

import java.util.Date
import cats._
import cats.implicits._

object MeetCats extends App {
  val showInt = Show.apply[Int]
  val showString = Show.apply[String]

  val intAsString: String = showInt.show(123)
  val stringAsString = showString.show("abc")


  123.show
  "abc".show


  implicit val dateShow: Show[Date] =
    Show.show(date => s"${date.getTime}ms since the epoch.")

  implicit val catShow: Show[Cat] =
    Show.show(cat => s"${cat.name} is a ${cat.age} year-old ${cat.color} cat.")
}

