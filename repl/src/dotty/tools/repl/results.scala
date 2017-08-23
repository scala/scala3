package dotty.tools
package repl

import dotc.reporting.diagnostic.MessageContainer

/** Contains the different data and type structures used to model results
 *  in the REPL
 */
object results {

  /** Type alias for `List[MessageContainer]` */
  type Errors = List[MessageContainer]

  /** Result is a type alias for an Either with left value `Errors` */
  type Result[+A] = scala.util.Either[Errors, A]

  implicit class ResultConversionA[A](val a: A) extends AnyVal {
    def result: Result[A] = scala.util.Right(a)
  }

  implicit class ResultConversionErr(val xs: Errors) extends AnyVal {
    def errors[A]: Result[A] = scala.util.Left(xs)
  }
}
