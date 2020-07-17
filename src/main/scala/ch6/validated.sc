import cats.Semigroupal
import cats.data.Validated
import cats.instances.list._
import cats.syntax.validated._
import cats.syntax.apply._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}


type AllErrorsOr[A] = Validated[List[String], A]

Semigroupal[AllErrorsOr].product(
  Validated.invalid(List("Error 1")),
  Validated.invalid(List("Error 2")),
)

123.valid[List[String]]

List("Badness").invalid[Int]


import cats.syntax.applicative._ // pure
import cats.syntax.applicativeError._ // raiseError

123.pure[AllErrorsOr]

List("Badness").raiseError[AllErrorsOr, Int]


Validated.catchOnly[NumberFormatException]("foo".toInt)

Validated.catchNonFatal(sys.error("Badness"))

Validated.fromTry(scala.util.Try("foo".toDouble))

Validated.fromEither[String, Int](Left("Badness"))

Validated.fromOption[String, Int](None, "Badness")

import cats.instances.string._
import cats.instances.vector._

type StringErrorsOr[A] = Validated[String, A]
(
  Vector(404).invalid[Int],
  Vector(500).invalid[Int]
).tupled

import cats.data.NonEmptyVector

(
  NonEmptyVector.of("Error 1").invalid[Int],
  NonEmptyVector.of("Error 2").invalid[Int]
).tupled

(
  1.valid[NonEmptyVector[String]],
  NonEmptyVector.of("Error 2").invalid[Int]
).tupled

(
  1.valid[NonEmptyVector[String]],
  2.valid[NonEmptyVector[String]],
).tupled


import cats.syntax.either._

"Badness".invalid[Int]
"Badness".invalid[Int].toEither
"Badness".invalid[Int].toEither.toValidated


// for input from html (String, String)
case class User(name: String, age: Int)


// naive
//def readName(input: Map[String, String]): Either[List[String], String] =
//  input.get("name") match {
//    case Some(n) if n.length > 0 => Right(n)
//    case Some(n) if n.length == 0 => Left(List("name should not be empty"))
//    case None => Left(List("error: no name"))
//  }
//
//def readAge(input: Map[String, String]): Either[List[String], Int] =
//  input.get("age").map(_.toInt) match {
//    case Some(n) if n >= 0 => Right(n)
//    case Some(n) if n < 0 => Left(List("age should be non-negative"))
//    case None => Left(List("error: no age"))
//  }


// conbine with either
//def getValue(map: Map[String, String], key: String): Either[List[String], String] =
//  map.get(key) match {
//    case Some(x) => Right(x)
//    case None => Left(List(s"no $key in $map map"))
//  }
//
//def parseInt(s: String): Either[List[String], Int] =
//  Try {
//    s.toInt
//  } match {
//    case Success(x) => Right(x)
//    case Failure(_) => Left(List(s"cannot parse $s to Int"))
//  }
//
//def nonBlank(s: String): Either[List[String], String] =
//  if (s.isEmpty) Left(List("string should not be empty"))
//  else Right(s)
//
//def nonNegative(x: Int): Either[List[String], Int] =
//  if (x < 0) Left(List("the number should >= 0"))
//  else Right(x)
//
//
//def readNameE(input: Map[String, String]): Either[List[String], String] =
//  for {
//    x <- getValue(input, "name")
//    name <- nonBlank(x)
//  } yield name
//
//def readAgeE(input: Map[String, String]): Either[List[String], Int] =
//  for {
//    x <- getValue(input, "age")
//    num <- parseInt(x)
//    age <- nonNegative(num)
//  } yield age
//
//def readPersonE(input: Map[String, String]): Either[List[String], User] =
//  for {
//    name <- readNameE(input)
//    age <- readAgeE(input)
//  } yield User(name, age)
//
//readPersonE(Map(
//  "name" -> ""
//))
//
//readPersonE(Map(
//  "name" -> "",
//  "age" -> "-2"
//))
//
//readPersonE(Map(
//  "name" -> "Bob",
//  "age" -> "3"
//))


type UserErrorOr[A] = Validated[List[String], A]

def getValueV(map: Map[String, String], key: String): UserErrorOr[String] =
  map.get(key) match {
    case Some(x) => x.valid
    case None => List(s"no $key in $map map").invalid
  }

def parseIntV(s: String): UserErrorOr[Int] =
  Try {
    s.toInt
  } match {
    case Success(x) => x.valid
    case Failure(_) => List(s"cannot parse $s to Int").invalid
  }

def nonBlankV(s: String): UserErrorOr[String] =
  if (s.isEmpty) List("string should not be empty").invalid
  else s.valid

def nonNegativeV(x: Int): UserErrorOr[Int] =
  if (x < 0) List("the number should >= 0").invalid
  else x.valid


def readNameV(input: Map[String, String]): Validated[List[String], String] =
  getValueV(input, "name").andThen(nonBlankV)

def readAgeV(input: Map[String, String]): Validated[List[String], Int] =
  getValueV(input, "age").andThen(parseIntV).andThen(nonNegativeV)

def readPersonV(input: Map[String, String]): Validated[List[String], User] =
  (
    readNameV(input),
    readAgeV(input)
  ).mapN(User.apply)

readPersonV(Map(
  "name" -> ""
))

readPersonV(Map(
  "name" -> "",
  "age" -> "-2"
))

readPersonV(Map(
  "name" -> "Bob",
  "age" -> "3"
))

import cats.syntax.either._
type FormData = Map[String, String]
type FailFast[A] = Either[List[String], A]
type FailSlow[A] = Validated[List[String], A]

def getValue(name: String)(data: FormData): FailFast[String] =
  data.get(name).
    toRight(List(s"$name field not specified"))

val getName = getValue("name") _

def parseInt(name: String)(data: String): FailFast[Int] =
  Either.catchOnly[NumberFormatException](data.toInt).
    leftMap(_ => List(s"$name must be an integer"))

def nonBlank(name: String)(data: String): FailFast[String] =
  Right(data).
    ensure(List(s"$name cannot be blank"))(_.nonEmpty)

def nonNegative(name: String)(data: Int): FailFast[Int] =
  Right(data).
    ensure(List(s"$name must be non-negative"))(_ >= 0)

def readName(data:FormData): FailFast[String] =
  getValue("name")(data)
  .flatMap(nonBlank("name"))

def readAge(data: FormData): FailFast[Int] =
  getValue("age")(data)
  .flatMap(nonBlank("age"))
  .flatMap(parseInt("age"))
  .flatMap(nonNegative("age"))



import cats.instances.list._
import cats.syntax.apply._
def readUser(data: FormData): FailSlow[User] =
  (
    readName(data).toValidated,
    readAge(data).toValidated
  ).mapN(User.apply)


readUser(Map(
  "age" -> "-1"
))

