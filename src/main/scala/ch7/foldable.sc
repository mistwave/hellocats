import cats.Foldable
import cats.instances.string._
import cats.instances.list._
import cats.instances.int._


Foldable[List].foldMap(List(1,2,3))(_.toString)

import cats.syntax.foldable._

List(1,2,3).combineAll