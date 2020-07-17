package ch4

import scala.annotation.tailrec

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]


object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  val treeMonad = new cats.Monad[Tree] {
    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Branch(left, right) =>
          branch(flatMap(left)(f), flatMap(right)(f))
        case Leaf(value) =>
          f(value)
      }

    override def pure[A](x: A): Tree[A] =
      leaf(x)

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a)) {
        case Left(value) =>
          tailRecM(value)(f)
        case Right(value) =>
          Leaf(value)
      }
  }

}

object treeMonad {


}
