package scala.internal

import scala.quoted._

object Quoted {

  /** A term quote is desugared by the compiler into a call to this method */
  def exprQuote[T](x: T): Expr[T] =
    throw new Error("Internal error: this method call should have been replaced by the compiler")

  /** A term splice is desugared by the compiler into a call to this method */
  def exprSplice[T](x: Expr[T]): T =
    throw new Error("Internal error: this method call should have been replaced by the compiler")

  /** A type quote is desugared by the compiler into a call to this method */
  def typeQuote[T/*FIXME <: AnyKind*/]: Type[T] =
    throw new Error("Internal error: this method call should have been replaced by the compiler")

  /** A type splice is desugared by the compiler into a call to this type alias */
  type TypeSplice[T /*FIXME <: AnyKind*/, Typ <: Type[T] with Singleton] = T

  /** Temporary splice used durring typing to infere T and Typ */
  def typeSplice[T /*FIXME <: AnyKind*/, Typ <: Type[T] with Singleton](t: Typ): Type[T] =
    throw new Error("Internal error: this method call should have been replaced by the compiler")
}
