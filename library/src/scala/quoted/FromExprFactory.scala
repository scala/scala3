package scala.quoted

import language.experimental.captureChecking
import scala.deriving.Mirror

/** A factory that, given a `Type[T]`, produces a `FromExpr[T]`.
 *
 *  Like `ToExprFactory`, routes each field/case through `FromExprFactory` recursively, so
 *  `derives FromExprFactory` on a generic type does not require a manual
 *  `given [A: {Type, FromExpr}] => FromExpr[Box[A]]` for its type parameters.
 */
trait FromExprFactory[T]:
  def apply()(using Type[T]): FromExpr[T]

object FromExprFactory:
  inline def derived[T: Mirror.Of as m]: FromExprFactory[T] = inline m match
    case given Mirror.ProductOf[T] =>
      derivedProduct[T](compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, FromExprFactory]].toList.asInstanceOf[List[FromExprFactory[Any]]])
    case given Mirror.SumOf[T] =>
      derivedSum[T](summonAllOrDerive[m.MirroredElemTypes])

  private inline def summonAllOrDerive[Elems <: Tuple]: List[FromExprFactory[Any]] = inline compiletime.erasedValue[Elems] match
    case _: EmptyTuple => Nil
    case _: (h *: t) => summonOrDerive[h].asInstanceOf[FromExprFactory[Any]] :: summonAllOrDerive[t]

  private inline def summonOrDerive[T]: FromExprFactory[T] = compiletime.summonFrom:
    case fe: FromExprFactory[T] => fe
    case given Mirror.Of[T] => derived[T]

  private def derivedProduct[T: Mirror.ProductOf as m](elemInstances: -> List[FromExprFactory[Any]]): FromExprFactory[T] = new FromExprFactory[T]:
    private lazy val elems = elemInstances
    def apply()(using Type[T]): FromExpr[T] = new FromExpr[T]:
      def unapply(x: Expr[T])(using quotes: Quotes): Option[T] =
        import quotes.reflect.*
        val tpe = TypeRepr.of[T]
        val fieldSyms = tpe.typeSymbol.caseFields
        val resolvedElems = elems.zip(fieldSyms).map:
          case (factory, fieldSym) =>
            tpe.memberType(fieldSym).asType match
              case '[t] => factory.asInstanceOf[FromExprFactory[t]].apply().asInstanceOf[FromExpr[Any]]

        unapplyProduct[T](resolvedElems)(x)

  private def derivedSum[T: Mirror.SumOf](elemInstances: -> List[FromExprFactory[Any]]): FromExprFactory[T] = new FromExprFactory[T]:
    private lazy val elems = elemInstances
    def apply()(using Type[T]): FromExpr[T] = new FromExpr[T]:
      def unapply(x: Expr[T])(using quotes: Quotes): Option[T] =
        import quotes.reflect.*
        val caseSyms = TypeRepr.of[T].typeSymbol.children
        val resolvedElems: List[FromExpr[Any]] = elems.zip(caseSyms).map:
          case (factory, caseSym) =>
            caseSym.typeRef.asType match
              case '[c] => factory.asInstanceOf[FromExprFactory[c]].apply().asInstanceOf[FromExpr[Any]]
        unapplySum[T](resolvedElems)(x)

  /** Bridges any type that already has a plain `FromExpr` (e.g. `Int`, `String`) into `FromExprFactory`. */
  given [T] => (fe: FromExpr[T]) => FromExprFactory[T]:
    def apply()(using Type[T]): FromExpr[T] = fe

private[quoted] trait LowPriorityFromExpr:
  given [T: Type] => (f: FromExprFactory[T]) => FromExpr[T] = f.apply()
