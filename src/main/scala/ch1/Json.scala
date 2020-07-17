package ch1


sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json


trait JsonWriter[A] {
  def write(value: A): Json
}


final case class Person(name:String, email:String)

object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      override def write(value: String): Json = JsString(value)
    }

  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      override def write(value: Person): Json = JsObject {
        Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        )
      }
    }

  implicit val intWriter: JsonWriter[Int] = new JsonWriter[Int] {
    override def write(value: Int): Json = JsNumber(value)
  }

  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      override def write(value: Option[A]): Json = value match {
        case Some(aValue) => writer.write(aValue)
        case None => JsNull
      }
    }
}


object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

object Test extends App {
  import JsonWriterInstances._
  import JsonSyntax._


  println(Json.toJson(Person("yifu", "yifu@shopee.com")))
  println(Json.toJson(123))
  Person("yifu","haha.com").toJson

  println(implicitly[JsonWriter[String]])

  println(Json.toJson(Option("A option!")))
  println(Json.toJson(Option.empty[String]))
}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}