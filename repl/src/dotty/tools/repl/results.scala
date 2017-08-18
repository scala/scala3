package dotty.tools
package repl

import dotc.reporting.diagnostic.MessageContainer

/** Contains the different data and type structures used to model results
 *  in the REPL
 */
object results {

  /** Type alias for `List[MessageContainer]` */
  private type Errors = List[MessageContainer]
  /** Private ADT instead of using union type, we're not dotty yet... */
  private sealed trait Disjunction[+A]
  /** Successful version of `Disjunction[A]` */
  private case class Success[A](a: A) extends Disjunction[A]
  /** Erroneous version of `Disjunction[A]` */
  private case class ErrorContainer(messages: Errors) extends Disjunction[Nothing]

  /** A result of a computation is either a list of errors or the result `A`
   *
   *  @note should be replaced by the right-biased `Either` from 2.12.x, TODO
   */
  class Result[+A] private[results] (result: Disjunction[A]) { self =>

    def flatMap[B](f: A => Result[B]): Result[B] =
      result match {
        case ec: ErrorContainer => new Result(ec)
        case Success(a) => f(a)
      }

    def map[B](f: A => B): Result[B] =
      result match {
        case ec: ErrorContainer => new Result(ec)
        case Success(a) => new Result(Success(f(a)))
      }

    def fold[B](onErrors: Errors => B, onResult: A => B): B =
      result match {
        case ErrorContainer(errs) => onErrors(errs)
        case Success(a) => onResult(a)
      }

    def recoverWith[B >: A](pf: PartialFunction[Errors, Result[B]]): Result[B] =
      result match {
        case ErrorContainer(errs) if pf.isDefinedAt(errs) => pf(errs)
        case _ => this
      }

    def orElse[B >: A](res: => Result[B]): Result[B] =
      result match {
        case _: ErrorContainer => res
        case _ => this
      }

    class WithFilter(p: A => Boolean) {
      def flatMap[B](f: A => Result[B]) = self.flatMap(f)
      def map[B](f: A => B) = self.map(f)
    }

    def withFilter(p: A => Boolean) = new WithFilter(p)
  }

  implicit class ResultConversionA[A](val a: A) extends AnyVal {
    def result: Result[A] = new Result(Success(a))
  }

  implicit class ResultConversionErr(val xs: Errors) extends AnyVal {
    def errors[A]: Result[A] = new Result[A](ErrorContainer(xs))
  }
}
