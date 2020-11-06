package scala.internal.quoted

import scala.annotation.{Annotation, compileTimeOnly}

object Patterns {

  /** A splice in a quoted pattern is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternHole`")
  def patternHole[T]: T = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternHigherOrderHole`")
  /** A higher order splice in a quoted pattern is desugared by the compiler into a call to this method */
  def patternHigherOrderHole[U](pat: Any, args: Any*): U = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.higherOrderHole`")
  /** A higher order splice in a quoted pattern is desugared by the compiler into a call to this method */
  def higherOrderHole[U](args: Any*): U = ???

  /** A splice of a name in a quoted pattern is that marks the definition of a type splice */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternType`")
  class patternType extends Annotation

  /** A type pattern that must be aproximated from above */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.fromAbove`")
  class fromAbove extends Annotation

}
