package ch11

import scala.language.higherKinds

trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V

  def values[K, V](f: F[K, V]): List[V]

}

object KeyValueStore {
  implicit val mapStore: KeyValueStore[Map] = new KeyValueStore[Map] {
    override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
      f + (k -> v)

    override def get[K, V](f: Map[K, V])(k: K): Option[V] =
      f.get(k)

    override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
      f.getOrElse(k, default)

    override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(k: K, v: V)(implicit keyValueStore: KeyValueStore[F]): F[K, V] =
      keyValueStore.put(f)(k, v)

    def get(k: K)(implicit keyValueStore: KeyValueStore[F]): Option[V] =
      keyValueStore.get(f)(k)

    def getOrElse(k: K, default: V)(implicit keyValueStore: KeyValueStore[F]): V =
      keyValueStore.getOrElse(f)(k, default)

    def values(implicit keyValueStore: KeyValueStore[F]): List[V] =
      keyValueStore.values(f)
  }
}

