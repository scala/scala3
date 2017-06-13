package dotty.tools
package repl

import dotc.reporting.diagnostic.MessageContainer

object results {
  type Result[A] = Errors | A

  case class Errors(msgs: Seq[MessageContainer])

  implicit class RichResult[A](val res: Result[A]) /*extends AnyVal */{
    def map[B](f: A => B): Result[B] = res match {
      case errs: Errors => errs
      case a: A => f(a)
    }

    def flatMap[B](f: A => Result[B]): Result[B] = map(f)

    def fold[B](left: Errors => B, right: A => B): B = res match {
      case errors: Errors => left(errors)
      case a: A => right(a)
    }
  }

  def id[A](a: A): a.type = a
}
