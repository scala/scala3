package dotty.tools
package repl

import dotc.reporting.diagnostic.MessageContainer

object results {
  type Errors = Seq[MessageContainer]

  /** A result of a computation is either a list of errors or the result `A` */
  class Result[A] private[results] (result: Errors | A) { self =>
    def flatMap[B](f: A => Result[B]): Result[B] =
      result match {
        case errs: Errors => new Result(errs)
        case a: A @unchecked => f(a)
      }

    def map[B](f: A => B): Result[B] =
      result match {
        case errs: Errors => new Result(errs)
        case a: A @unchecked => new Result(f(a))
      }

    def fold[B](onErrors: Errors => B, onResult: A => B): B =
      result match {
        case errs: Errors => onErrors(errs)
        case a: A @unchecked => onResult(a)
      }

    class WithFilter(p: A => Boolean) {
      def flatMap[B](f: A => Result[B]) = self.flatMap(f)
      def map[B](f: A => B) = self.map(f)
    }

    def withFilter(p: A => Boolean) = new WithFilter(p)
  }

  implicit class ResultConversionA[A](val a: A) extends AnyVal {
    def result: Result[A] = new Result(a)
  }

  implicit class ResultConversionErr(val xs: Errors) extends AnyVal {
    def errors[A]: Result[A] = new Result[A](xs)
  }
}
