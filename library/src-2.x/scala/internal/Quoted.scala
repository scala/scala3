package scala.internal

import scala.annotation.Annotation
import scala.quoted._

object Quoted {

  /** A term quote is desugared by the compiler into a call to this method */
  def exprQuote[T](x: T): Expr[T] =
    throw new Error("Internal error: this method call should have been replaced by the compiler")

  /** A term splice is desugared by the compiler into a call to this method */
  def exprSplice[T](x: Expr[T]): T =
    throw new Error("Internal error: this method call should have been replaced by the compiler")

  /** A type quote is desugared by the compiler into a call to this method */
  def typeQuote[T/* <: AnyKind */]: Type[T] =
    throw new Error("Internal error: this method call should have been replaced by the compiler")

  /** A splice in a quoted pattern is desugared by the compiler into a call to this method */
  def patternHole[T]: T =
    throw new Error("Internal error: this method call should have been replaced by the compiler")

  /** A splice of a name in a quoted pattern is desugared by wrapping getting this annotation */
  class patternBindHole extends Annotation

}
