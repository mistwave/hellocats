package ch4

import cats.syntax.either._

sealed trait LoginError extends Product with Serializable

final case class UserNotFound(username: String) extends LoginError
final case class PasswordIncorrect(username: String) extends LoginError
case object UnexcepedError extends LoginError




object errors extends App{
  case class User(username: String ,password: String)
  type LoginResult = Either[LoginError, User]


  def handError(error: LoginError): Unit = error match {
    case UserNotFound(username) =>
      println(s"user not found: $username")
    case PasswordIncorrect(username) =>
      println(s"password incorrect: $username")
    case UnexcepedError =>
      println(s"Unexpected error")
  }


  val result1: LoginResult = User("dave", "passw0rd").asRight
  val result2: LoginResult = UserNotFound("dave").asLeft

  result1.fold(handError, println)
  result2.fold(handError, println)
}
