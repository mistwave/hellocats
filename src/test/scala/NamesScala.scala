object NamesScala extends App {

  import scala.collection.parallel.CollectionConverters._

  val names = List("a", "bob", "mike", "i")

  names
    .par
    .filter(s => s.size > 1)
    .map(s => s.substring(0, 1).toUpperCase() +
      s.substring(1))
    .foreach(println)




  def compose[A, B, C](f: A => B, g: B => C): A => C =
    (a: A) => g(f(a))


}

