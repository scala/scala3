package scala.internal.quoted

import scala.annotation.{Annotation, compileTimeOnly}
import scala.quoted._

@compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime`")
object CompileTime {

  /** A term quote is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.exprQuote`")
  def exprQuote[T](x: T): QuoteContext ?=> Expr[T] = ???

  /** A term splice is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.exprSplice`")
  def exprSplice[T](x: QuoteContext ?=> Expr[T]): T = ???

  /** A term splice nested within a quote is desugared by the compiler into a call to this method.
   *  `ctx` is the `QuoteContext` that the quote of this splice uses.
   */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.exprNestedSplice`")
  def exprNestedSplice[T](ctx: QuoteContext)(x: ctx.Nested ?=> Expr[T]): T = ???

  /** Artifact of pickled type splices
   *
   *  During quote reification a quote `'{ ... F[$t] ... }` will be transformed into
   *  `'{ @quoteTypeTag type T$1 = $t ... F[T$1] ... }` to have a tree for `$t`.
   *  This artifact is removed during quote unpickling.
   *
   *  See PickleQuotes.scala and PickledQuotes.scala
   */
  @compileTimeOnly("Illegal reference to `scala.internal.quoted.CompileTime.quoteTypeTag`")
  class quoteTypeTag extends Annotation

}
