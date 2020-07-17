package ch8

import cats.{Applicative, Id}
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

class Uptime {

}

class UptimeService[F[_]](client: UptimeClient[F])(implicit applicativeF: Applicative[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

trait TestUptimeClient extends UptimeClient[Id] {
  def getUptime(hostname: String): Int
}

class TestClient(hosts: Map[String, Int]) extends TestUptimeClient {
  override def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
}

class RealClient(hosts: List[String]) extends UptimeClient[Future] {
  override def getUptime(hostname: String): Future[Int] = ???
}

object Test extends App{
  def testTotalUpTime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }
}