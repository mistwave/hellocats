package ch2

import cats._
import cats.implicits._

object Monoids extends App {
  val booleanWithAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  val booleanWithOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  def setWithUnion[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]

    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  def setWithIntersection[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
  }

}

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
}


object MonoidInCat extends App {

  println(cats.Monoid[String].combine("hello", "world"))
  println(cats.Monoid[String].empty)

  println("Hi " |+| "there" |+| cats.Monoid[String].empty)

}

// ex2.5.4
object SuperAdder extends App {
  def add[A: cats.Monoid](items: List[A]): A =
    items.fold(cats.Monoid[A].empty)(_ |+| _)


  val orderMonoid: cats.Monoid[Order] = new cats.Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order =
      Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  implicit val orderMonoid2: cats.Monoid[Order] =
    cats.Monoid.instance[Order](Order(0, 0),
      (a, b) => Order(a.totalCost + b.totalCost, a.quantity + b.quantity))

  implicit val orderEq: Eq[Order] = Eq.instance[Order] { (x, y) =>
    x.totalCost === y.totalCost && x.quantity === y.quantity
  }
  case class Order(totalCost: Double, quantity: Double)



  assert(6 === add(List(1, 2, 3)))
  assert(Option(6) === add(List(Some(1), None, Some(2), None, Some(3))))
  assert(Order(6,3) === add(List(Order(1, 2), Order(5, 1))))
}
