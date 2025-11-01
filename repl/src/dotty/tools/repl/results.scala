package dotty.tools
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

  extension [A](a: A)
    def result: Result[A] = scala.util.Right(a)

  extension [A](xs: Errors)
    def errors: Result[A] = scala.util.Left(xs)

}
