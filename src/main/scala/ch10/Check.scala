package ch10

import cats.Semigroup
import cats.data.Validated


sealed trait Check[E, A, B] {

  import Check._

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](func: B => C): Check[E, A, C] =
    Map[E, A, B, C](this, func)

  def flatMap[C](f: B => Check[E, A, C]) =
    FlatMap[E, A, B, C](this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] =
    AndThen[E, A, B, C](this, that)
}

object Check {

  final case class Map[E, A, B, C](check: Check[E, A, B],
                                   f: B => C
                                  ) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).map(f)
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B],
                                       f: B => Check[E, A, C]) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => f(b)(a).toEither))

  }

  final case class AndThen[E, A, B, C](ca: Check[E, A, B],
                                       cb: Check[E, B, C]
                                      ) extends Check[E, A, C] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      ca(a).withEither(_.flatMap(b => cb(b).toEither))
  }

  final case class Pure[E, A, B](func: A => Validated[E, B]) extends Check[E, A, B] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] =
      func(a)
  }

  final case class PurePredicate[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(a)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(pred)

  def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] =
    Pure(func)
}

