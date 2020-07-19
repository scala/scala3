package dottyBench.tools
package repl

import dotc.reporting.Diagnostic

/** Contains the different data and type structures used to model results
 *  in the REPL
 */
object results {

  /** Type alias for `List[Diagnostic]` */
  type Errors = List[Diagnostic]

  /** Result is a type alias for an Either with left value `Errors` */
  type Result[+A] = scala.util.Either[Errors, A]

  implicit class ResultConversionA[A](val a: A) extends AnyVal {
    def result: Result[A] = scala.util.Right(a)
  }

  implicit class ResultConversionErr(val xs: Errors) extends AnyVal {
    def errors[A]: Result[A] = scala.util.Left(xs)
  }
}
