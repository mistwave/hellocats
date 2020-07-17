import cats.Semigroupal
import cats.instances.option._
import cats.syntax.apply._


Semigroupal[Option].product(Some(123), Some("abc"))
Semigroupal[Option].product(None, Some("abc"))
Semigroupal[Option].product(Some(123), None)


Semigroupal.tuple3(Option(1), Some("ab"), Some(4))

(Option(123), Option("abc")).tupled

case class Cat(name: String, born: Int, foods: List[String])

import cats.Monoid
import cats.instances.int._
import cats.instances.invariant._
import cats.instances.list._
import cats.instances.string._
import cats.syntax.apply._
import cats.syntax.semigroup._

val tupleToCat: (String, Int, List[String]) => Cat =
  Cat.apply _

val catToTuple: Cat => (String, Int, List[String]) =
  cat => (cat.name, cat.born, cat.foods)

implicit val catMonoid: Monoid[Cat] = (
  Monoid[String],
  Monoid[Int],
  Monoid[List[String]]
).imapN(tupleToCat)(catToTuple)

val garfield = Cat("Garfield", 1978, List("Lasagne"))
val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

garfield |+| heathcliff


import cats.instances.future._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

val futurePair: Future[(String, Int)] = Semigroupal[Future].
  product(Future("Hello"), Future(123))

Await.result(futurePair, 1.second)


val futureCat = (
  Future("Garfield"),
  Future(1978),
  Future(List("Lasagne"))
).mapN(Cat.apply)

Await.result(futureCat, 1.second)


import cats.instances.list._
import cats.instances.either._

Semigroupal[List].product(List(1, 2), List(3, 4))

type ErrorOr[A] = Either[List[String], A]

Semigroupal[ErrorOr].product(
  Left(List("Error 1")),
  Left(List("Error 2"))
)

import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._


def product[M[_] : Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
//  x.flatMap(a => y.map(b => (a, b)))
//  x.map2(y)((_, _))
  for {
    a <- x
    b <- y
  } yield (a, b)
