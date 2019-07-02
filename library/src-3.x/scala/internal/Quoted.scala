package scala.internal

import scala.annotation.{Annotation, compileTimeOnly}
import scala.quoted._

object Quoted {

  /** A term quote is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.internal.Quoted.exprQuote`")
  def exprQuote[T](x: T): Expr[T] = ???

  /** A term splice is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.internal.Quoted.exprSplice`")
  def exprSplice[T](x: Expr[T]): T = ???

  /** A type quote is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.internal.Quoted.typeQuote`")
  def typeQuote[T <: AnyKind]: Type[T] = ???

  /** A splice in a quoted pattern is desugared by the compiler into a call to this method */
  @compileTimeOnly("Illegal reference to `scala.internal.Quoted.patternHole`")
  def patternHole[T]: T = ???

  /** A splice of a name in a quoted pattern is desugared by wrapping getting this annotation */
  @compileTimeOnly("Illegal reference to `scala.internal.Quoted.patternBindHole`")
  class patternBindHole extends Annotation

  /** Artifact of pickled type splices
   *
   *  During quote reification a quote `'{ ... F[$t] ... }` will be transformed into
   *  `'{ @quoteTypeTag type T$1 = $t ... F[T$1] ... }` to have a tree for `$t`.
   *  This artifact is removed durring quote unpickling.
   *
   *  See ReifyQuotes.scala and PickledQuotes.scala
   */
  @compileTimeOnly("Illegal reference to `scala.internal.Quoted.patternBindHole`")
  class quoteTypeTag extends Annotation

}
