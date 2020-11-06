package scala.internal.quoted

import scala.quoted._

/** Pickled representation of a quoted expression or type */
trait PickledQuote:

  /** Bytes of the TASTy containing the quoted expression or type */
  def bytes(): Array[Byte]

  /** Expression that will fill the hole `Hole(<idx> | <args>*)` */
  def exprSplice(idx: Int)(args: Seq[Any /* Expr[Any] | Type[?] */])(qctx: QuoteContext): Expr[Any]

  /** Type that will fill the hole `Hole(<idx> | <args>*)` */
  def typeSplice(idx: Int)(args: Seq[Any /* Expr[Any] | Type[?] */]): Type[?]

object PickledQuote:

  def unpickleExpr[T](pickledQuote: PickledQuote): QuoteContext ?=> Expr[T] =
    qctx.asInstanceOf[QuoteContextInternal].unpickleExpr(pickledQuote).asInstanceOf[Expr[T]]

  def unpickleType[T](pickledQuote: PickledQuote): QuoteContext ?=> Type[T] =
    qctx.asInstanceOf[QuoteContextInternal].unpickleType(pickledQuote).asInstanceOf[Type[T]]

  /** Create an instance of PickledExpr from encoded tasty and sequence of labmdas to fill holes
   *
   *  @param pickled: Bytes of tasty encoded using TastyString.pickle
   *  @param seq: Sequence containing all the functions needed to fill the holes of the quote
   */
  def make(pickled: List[String], seq: Seq[Seq[Any /* Expr[Any] | Type[?] */] => Any]): PickledQuote =
    // TODO: generate a more efficient representation
    //       - avoid creation of lambdas
    //       - use swich on id
    new PickledQuote:
      def bytes(): Array[Byte] =
        TastyString.unpickle(pickled)
      def exprSplice(idx: Int)(args: Seq[Any])(qctx: QuoteContext): Expr[Any] =
        seq(idx)(args).asInstanceOf[QuoteContext => Expr[Any]](qctx)
      def typeSplice(idx: Int)(args: Seq[Any]): Type[?] =
        seq(idx)(args).asInstanceOf[Type[?]]
