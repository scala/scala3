package scala.internal.quoted

import scala.annotation.{Annotation, compileTimeOnly}
import scala.quoted._

@compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime`")
object CompileTime {

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.exprQuote`")
  def exprQuote[T](x: T): QuoteContext ?=> Expr[T] = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.exprSplice`")
  def exprSplice[T](x: QuoteContext ?=> Expr[T]): T = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.exprNestedSplice`")
  def exprNestedSplice[T](ctx: QuoteContext)(x: ctx.Nested ?=> Expr[T]): T = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.typeQuote`")
  def typeQuote[T <: AnyKind]: Staged[T] = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternHole`")
  def patternHole[T]: T = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternBindHole`")
  class patternBindHole extends Annotation

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.patternType`")
  class patternType extends Annotation

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.fromAbove`")
  class fromAbove extends Annotation

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.quoteTypeTag`")
  class quoteTypeTag extends Annotation

}
