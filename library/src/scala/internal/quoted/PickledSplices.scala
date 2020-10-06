package scala.internal.quoted

import scala.quoted._

/** Provider of expressions and types that will fill the holes a pickled quote */
trait PickledSplices:

  /** Expression that will fill the hole `Hole(<idx> | <args>*)` */
  def exprSplice(idx: Int)(args: Seq[Any /* Expr[Any] | Type[?] */])(qctx: QuoteContext): Expr[Any]

  /** Type that will fill the hole `Hole(<idx> | <args>*)` */
  def typeSplice(idx: Int)(args: Seq[Any /* Expr[Any] | Type[?] */]): Type[?]

object PickledSplices:
  // TODO: generate a more efficient representation
  //       - avoid creation of lambdas
  //       - use swich on id
  def make(seq: Seq[Seq[Any /* Expr[Any] | Type[?] */] => Any]): PickledSplices =
    new PickledSplices:
      def exprSplice(idx: Int)(args: Seq[Any])(qctx: QuoteContext): Expr[Any] =
        seq(idx)(args).asInstanceOf[QuoteContext => Expr[Any]](qctx)
      def typeSplice(idx: Int)(args: Seq[Any]): Type[?] =
        seq(idx)(args).asInstanceOf[Type[?]]
