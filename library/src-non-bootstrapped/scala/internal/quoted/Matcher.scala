package scala.internal.quoted

import scala.annotation.internal.sharable
import scala.annotation.{Annotation, compileTimeOnly}

import scala.quoted._

object Matcher {

  /** A splice in a quoted pattern is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternHole`")
  def patternHole[T]: T = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternHigherOrderHole`")
  /** A higher order splice in a quoted pattern is desugared by the compiler into a call to this method */
  def patternHigherOrderHole[U](pat: Any, args: Any*): U = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.higherOrderHole`")
  /** A higher order splice in a quoted pattern is desugared by the compiler into a call to this method */
  def higherOrderHole[U](args: Any*): U = ???

  // TODO remove
  /** A splice of a name in a quoted pattern is desugared by wrapping getting this annotation */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternBindHole`")
  class patternBindHole extends Annotation

  /** A splice of a name in a quoted pattern is that marks the definition of a type splice */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternType`")
  class patternType extends Annotation

  /** A type pattern that must be aproximated from above */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.fromAbove`")
  class fromAbove extends Annotation

  class QuoteMatcher[QCtx <: QuoteContext & Singleton](using val qctx: QCtx) {
    import qctx.tasty._

    def termMatch(scrutineeTerm: Term, patternTerm: Term, hasTypeSplices: Boolean): Option[Tuple] =
      throw new Exception("Non bootstrapped lib")

    def typeTreeMatch(scrutineeTypeTree: TypeTree, patternTypeTree: TypeTree, hasTypeSplices: Boolean): Option[Tuple] =
      throw new Exception("Non bootstrapped lib")
  }

}
