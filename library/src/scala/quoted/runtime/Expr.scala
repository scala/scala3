package scala.quoted
package runtime

import language.experimental.captureChecking

import scala.annotation.{Annotation, compileTimeOnly}

@compileTimeOnly("Illegal reference to `scala.quoted.runtime.Expr`")
object Expr:

  /** A term quote is desugared by the compiler into a call to this method
   *
   *  Calling this method in source has undefined behavior at compile-time
   */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Expr.quote`")
  def quote[T](x: T): Quotes ?=> scala.quoted.Expr[T] = ???

  /** A term splice is desugared by the compiler into a call to this method
   *
   *  Calling this method in source has undefined behavior at compile-time
   */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Expr.splice`")
  def splice[T](x: Quotes ?=> scala.quoted.Expr[T]): T = ???

  /** A term splice nested within a quote is desugared by the compiler into a call to this method.
   *  `ctx` is the `Quotes` that the quote of this splice uses.
   *
   *  Calling this method in source has undefined behavior at compile-time
   */
  @compileTimeOnly("Illegal reference to `scala.quoted.runtime.Expr.nestedSplice`")
  def nestedSplice[T](q: Quotes)(x: q.Nested ?=> scala.quoted.Expr[T]): T = ???
