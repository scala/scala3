package scala.quoted
package runtime

import scala.annotation.{Annotation, compileTimeOnly}

@compileTimeOnly("Illegal reference to `scala.quoted.runtime.Expr`")
object Expr:

  /** A term quote is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Expr.quote`")
  def quote[T](x: T): QuoteContext ?=> scala.quoted.Expr[T] = ???

  /** A term splice is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Expr.splice`")
  def splice[T](x: QuoteContext ?=> scala.quoted.Expr[T]): T = ???

  /** A term splice nested within a quote is desugared by the compiler into a call to this method.
  *  `ctx` is the `QuoteContext` that the quote of this splice uses.
  */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Expr.nestedSplice`")
  def nestedSplice[T](ctx: QuoteContext)(x: ctx.Nested ?=> scala.quoted.Expr[T]): T = ???
