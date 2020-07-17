package ch3

import cats.Functor
import cats.syntax.functor._

import scala.language.higherKinds

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree extends App{
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))

      case Leaf(value) => Leaf(f(value))
    }
  }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)


  println(Tree.leaf(100).map(_ + 2))


}

