package ch3

trait Codec[A] {
  self =>
  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] =
    new Codec[B] {
      override def encode(value: B): String = self.encode(enc(value))

      override def decode(value: String): B = dec(self.decode(value))
    }
}

object Codec extends App{
  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(value: String): String = value

    override def decode(value: String): String = value
  }

  implicit val intCodec: Codec[Integer] = stringCodec.imap(
    _.toInt, _.toString
  )
  implicit val doubleCodec: Codec[Double] = stringCodec.imap(
    _.toDouble, _.toString
  )

  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(
    _.toBoolean, _.toString
  )

  implicit def boxCodec[A](implicit aCodec: Codec[A]): Codec[Box[A]] =
    aCodec.imap(Box(_), _.value)

  List(
    encode(123.4),
    decode[Double]("123.4"),
    encode(Box(123.4)),
    decode[Box[Double]]("123.4")

  ).foreach(println)
}
