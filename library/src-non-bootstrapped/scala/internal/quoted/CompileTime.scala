package scala.internal.quoted

import scala.annotation.{Annotation, compileTimeOnly}
import scala.quoted._

@compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime`")
object CompileTime {

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.exprQuote`")
  def exprQuote[T](x: T): Quotes ?=> Expr[T] = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.exprSplice`")
  def exprSplice[T](x: Quotes ?=> Expr[T]): T = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.exprNestedSplice`")
  def exprNestedSplice[T](ctx: Quotes)(x: ctx.Nested ?=> Expr[T]): T = ???

  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.quoteTypeTag`")
  class quoteTypeTag extends Annotation

}
