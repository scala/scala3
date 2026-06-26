package scala.quoted.runtime

import language.experimental.captureChecking

import scala.quoted.{Quotes, Expr, Type}

/** Part of the `Quotes` interface that needs to be implemented by the compiler but is not visible to users. */
trait QuoteUnpickler:

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `holes`
   *
   *  Generated for code compiled with Scala 3.0.x and 3.1.x
   *
   *  @tparam T the type of the expression being unpickled
   *  @param pickled the pickled representation of the expression tree, as a single string or a list of strings
   *  @param typeHole a function that resolves type splice nodes by index and captured arguments
   *  @param termHole a function that resolves term splice nodes by index, with captured arguments and a `Quotes` context
   *  @return the unpickled `Expr` tree with splice holes replaced by the results of `typeHole` and `termHole`
   */
  def unpickleExpr[T](pickled: String | List[String], typeHole: (Int, Seq[Any]) => Type[?], termHole: (Int, Seq[Any], Quotes) => Expr[?]): scala.quoted.Expr[T]

  /** Unpickle `repr` which represents a pickled `Expr` tree,
   *  replacing splice nodes with `holes`.
   *
   *  Generated for code compiled with Scala 3.2.0+
   *
   *  @tparam T the type of the expression being unpickled
   *  @param pickled the pickled representation of the expression tree, as a single string or a list of strings
   *  @param types the types used in splice nodes, or `null` if there are none
   *  @param termHole a function that resolves term splice nodes by index, with spliced types/expressions and a `Quotes` context, or `null` if there are none
   *  @return the unpickled `Expr` tree with type splice holes replaced by the supplied `types` and term splice holes replaced by `termHole` results
   */
  def unpickleExprV2[T](pickled: String | List[String], types: Null | Seq[Type[?]], termHole: Null | ((Int, Seq[Type[?] | Expr[Any]], Quotes) => Expr[?])): scala.quoted.Expr[T]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `holes`
   *
   *  Generated for code compiled with Scala 3.0.x and 3.1.x
   *
   *  @tparam T the type being unpickled, which may be of any kind (e.g., `*`, `* => *`)
   *  @param pickled the pickled representation of the type tree, as a single string or a list of strings
   *  @param typeHole a function that resolves type splice nodes by index and captured arguments
   *  @param termHole a function that resolves term splice nodes by index, with captured arguments and a `Quotes` context
   *  @return the unpickled `Type` tree with splice holes replaced by the results of `typeHole` and `termHole`
   */
  def unpickleType[T <: AnyKind](pickled: String | List[String], typeHole: (Int, Seq[Any]) => Type[?], termHole: (Int, Seq[Any], Quotes) => Expr[?]): scala.quoted.Type[T]

  /** Unpickle `repr` which represents a pickled `Type` tree,
   *  replacing splice nodes with `holes`
   *
   *  Generated for code compiled with Scala 3.2.0+
   *
   *  @tparam T the type being unpickled, which may be of any kind (e.g., `*`, `* => *`)
   *  @param pickled the pickled representation of the type tree, as a single string or a list of strings
   *  @param types the types used in splice nodes, or `null` if there are none
   *  @return the unpickled `Type` tree with splice holes replaced by the supplied `types`
   */
  def unpickleTypeV2[T <: AnyKind](pickled: String | List[String], types: Null | Seq[Type[?]]): scala.quoted.Type[T]
