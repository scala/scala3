package scala.quoted
package internal

import scala.annotation.{Annotation, compileTimeOnly}

@compileTimeOnly("Illegal reference to `scala.quoted.internal.Expr`")
object Expr:

  /** A term quote is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.quoted.internal.Expr.quote`")
  def quote[T](x: T): Quotes ?=> scala.quoted.Expr[T] = ???

  /** A term splice is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.quoted.internal.Expr.splice`")
  def splice[T](x: Quotes ?=> scala.quoted.Expr[T]): T = ???

  /** A term splice nested within a quote is desugared by the compiler into a call to this method.
  *  `ctx` is the `Quotes` that the quote of this splice uses.
  */
  @compileTimeOnly("Illegal reference to `scala.quoted.internal.Expr.nestedSplice`")
  def nestedSplice[T](ctx: Quotes)(x: ctx.Nested ?=> scala.quoted.Expr[T]): T = ???
