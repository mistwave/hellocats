package ch3

trait Printable[A] {
  self =>
  def format(value: A): String

  def contramap[B](f: B => A): Printable[B] =
    new Printable[B] {
      override def format(value: B): String =
        self.format(f(value))
    }

}

final case class Box[A](value: A)

object Printable extends App {
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      override def format(value: String): String = "\"" + value + "\"";
    }

  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      override def format(value: Boolean): String = if (value) "yes" else "no"
    }

  implicit def boxPrintable[A](implicit aPrintable: Printable[A]): Printable[Box[A]] =
    aPrintable.contramap(_.value)

  List(
    format("hello"),
    format(1 == 2),
    format(Box("aaa in box"))
  ).foreach(println)
}