package ch1


trait Printable[A] {
  def format(value: A): String
}



object PrintableInstances {
  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = Int.toString()
  }
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = value
  }
  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(cat: Cat): String = {
      val name = Printable.format(cat.name)
      val age = Printable.format(cat.age)
      val color = Printable.format(cat.color)
      s"$name is a $age year-old $color cat."
    }
  }
}

object Printable {
  def format[A](value: A)(implicit ins: Printable[A]): String =
    ins.format(value)

  def print[A](value: A)(implicit ins: Printable[A]): Unit =
    println(format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit ins: Printable[A]): String = Printable.format(value)
    def print(implicit ins: Printable[A]): Unit = Printable.print(value)
  }

}
object TestCat extends App {
  import PrintableInstances._
  import PrintableSyntax._

//  Printable.print(cat)

}