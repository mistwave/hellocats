package ch4

import cats.Eval

object evals extends App{
  def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = as match {
    case head :: tail =>
      f(head, foldRight(tail, acc)(f))
    case Nil => acc
  }

  def foldRightSafe[A, B](as: List[A], acc: B)(f: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(f(a, _))
    }.value

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(f(head, foldRightEval(tail, acc)(f)))
      case Nil => acc
    }

//  foldRight((1 to 100000).toList, 0)(_ + _)
  // stackoverflow

  println(foldRightSafe((1 to 100000).toList, 0L)(_ + _))

}
