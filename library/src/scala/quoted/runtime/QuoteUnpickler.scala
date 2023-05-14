package scala.quoted.runtime

import scala.quoted.{Quotes, Expr, Type}

/** Part of the Quotes interface that needs to be implemented by the compiler but is not visible to users */
trait QuoteUnpickler:

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `holes`
   *
   *  Generated for code compiled with Scala 3.0.x and 3.1.x
   */
  def unpickleExpr[T](pickled: String | List[String], typeHole: (Int, Seq[Any]) => Type[?], termHole: (Int, Seq[Any], Quotes) => Expr[?]): scala.quoted.Expr[T]

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `holes`.
   *
   *  Generated for code compiled with Scala 3.2.0+
   */
  def unpickleExprV2[T](pickled: String | List[String], types: Null | Seq[Type[?]], termHole: Null | ((Int, Seq[Type[?] | Expr[Any]], Quotes) => Expr[?])): scala.quoted.Expr[T]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `holes`
   *
   *  Generated for code compiled with Scala 3.0.x and 3.1.x
   */
  def unpickleType[T <: AnyKind](pickled: String | List[String], typeHole: (Int, Seq[Any]) => Type[?], termHole: (Int, Seq[Any], Quotes) => Expr[?]): scala.quoted.Type[T]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `holes`
   *
   *  Generated for code compiled with Scala 3.2.0+
   */
  def unpickleTypeV2[T <: AnyKind](pickled: String | List[String], types: Null | Seq[Type[?]]): scala.quoted.Type[T]
