package scala.quoted.internal

/** Implementation of scala.quoted.Expr that sould only be extended by the implementation of `QuoteContext` */
abstract class Expr[+T] extends scala.quoted.Expr[T]
