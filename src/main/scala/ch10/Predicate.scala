package ch10

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.kernel.Semigroup
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.syntax.apply._

import cats.syntax.validated._

sealed trait Predicate[E, A] {
  import Predicate._
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
    this match {
      case Pure(f) => f(a)
      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)

      case Or(left, right) =>
        left(a) match {
          case Valid(a) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(a) => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

  def run(implicit s: Semigroup[E]): A => Either[E, A] =
    a => this(a).toEither
}


object Predicate{
  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if(fn(a)) a.valid else err.invalid)

  // function implementation
  final case class CheckF[E, A](f: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = f(a)

    def and(that: CheckF[E, A])(implicit eSemigroup: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this (a), that(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Right(_), Left(e)) => e.asLeft
          case (Left(e), Right(_)) => e.asLeft
          case (Right(_), Right(_)) => a.asRight
        }
      }
  }
}


//
//
//object CheckFTest extends App {
//
//  import cats.instances.list._
//
//  val a: CheckF[List[String], Int] =
//    CheckF { v =>
//      if (v > 2) v.asRight
//      else List("Must be > 2").asLeft
//    }
//
//  val b: CheckF[List[String], Int] =
//    CheckF { v =>
//      if (v < -2) v.asRight
//      else List("Must be < -2").asLeft
//    }
//
//  val check: CheckF[List[String], Int] = a and b
//
//  List(
//    check(5),
//    check(0),
//  ).foreach(println)
//}