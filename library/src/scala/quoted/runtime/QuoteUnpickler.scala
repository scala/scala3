package scala.quoted.runtime

import scala.quoted.{Quotes, Expr, Type}

/** Part of the Quotes interface that needs to be implemented by the compiler but is not visible to users */
trait QuoteUnpickler:

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `holes`
   */
  def unpickleExpr[T](pickled: String | List[String], typeHole: (Int, Seq[Any]) => Type[?], termHole: (Int, Seq[Any], Quotes) => Expr[?]): scala.quoted.Expr[T]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `holes`
   */
  def unpickleType[T <: AnyKind](pickled: String | List[String], typeHole: (Int, Seq[Any]) => Type[?], termHole: (Int, Seq[Any], Quotes) => Expr[?]): scala.quoted.Type[T]

