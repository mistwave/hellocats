package ch11

import cats.instances.map._
import cats.kernel.CommutativeMonoid
import cats.syntax.semigroup._

import scala.language.higherKinds


trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V])(f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter extends App{
  import KeyValueStore._
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) = counter


  implicit def grounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]): GCounter[F, K ,V] =
    new GCounter[F, K ,V] {
      override def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
        val value: V = f.getOrElse(k, m.empty) |+| v
        f.put(k, value)
      }

      override def merge(f1: F[K, V])(f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2

      override def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V =
        f.values.fold(m.empty)(m.combine)
    }


  implicit def mapGCounter[A]: GCounter[Map, String, A] = new GCounter[Map, String, A] {
    override def increment(f: Map[String, A])(k: String, v: A)(implicit m: CommutativeMonoid[A]): Map[String, A] = {
      val value = v |+| f.getOrElse(k, m.empty)
      f + (k -> value)
    }

    override def merge(f1: Map[String, A])(f2: Map[String, A])(implicit b: BoundedSemiLattice[A]): Map[String, A] =
      f1 |+| f2

    override def total(f: Map[String, A])(implicit m: CommutativeMonoid[A]): A =
      f.values.fold(m.empty)(m.combine)
  }


  import cats.instances.int._

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  val counter = grounterInstance[Map, String, Int]

  val merged = counter.merge(g1)(g2)

  val total = counter.total(merged)

  println(merged, total)
}





