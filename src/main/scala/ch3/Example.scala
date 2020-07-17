package ch3

import cats.Monoid
import cats.instances.string._
import cats.syntax.invariant._
import cats.syntax.semigroup._


object Example {
  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol(_))(_.name)

}
