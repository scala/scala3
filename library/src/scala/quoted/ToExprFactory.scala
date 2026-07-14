package scala.quoted

import language.experimental.captureChecking
import scala.deriving.Mirror

/** A factory that, given a `Type[T]`, produces a `ToExpr[T]`.
 *
 *  `ToExprFactory.derived` routes each field/case through `ToExprFactory` recursively, so
 *  `derives ToExprFactory` on a generic type does not require a manual
 *  `given [A: {Type, ToExpr}] => ToExpr[Box[A]]` for its type parameters.
 */
trait ToExprFactory[T]:
  def apply()(using Type[T]): ToExpr[T]

object ToExprFactory:
  inline def derived[T: Mirror.Of as m]: ToExprFactory[T] = inline m match
    case given Mirror.ProductOf[T] =>
      derivedProduct[T](compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, ToExprFactory]].toList.asInstanceOf[List[ToExprFactory[Any]]])
    case given Mirror.SumOf[T] =>
      derivedSum[T](summonAllOrDerive[m.MirroredElemTypes])

  private inline def summonAllOrDerive[Elems <: Tuple]: List[ToExprFactory[Any]] = inline compiletime.erasedValue[Elems] match
    case _: EmptyTuple => Nil
    case _: (h *: t) => summonOrDerive[h].asInstanceOf[ToExprFactory[Any]] :: summonAllOrDerive[t]

  private inline def summonOrDerive[T]: ToExprFactory[T] = compiletime.summonFrom:
    case te: ToExprFactory[T] => te
    case given Mirror.Of[T] => derived[T]

  private def derivedProduct[T: Mirror.ProductOf](elemInstances: -> List[ToExprFactory[Any]]): ToExprFactory[T] = new ToExprFactory[T]:
    private lazy val elems = elemInstances
    def apply()(using Type[T]): ToExpr[T] = new ToExpr[T]:
      def apply(x: T)(using Quotes): Expr[T] =
        import quotes.reflect.*
        val tpe = TypeRepr.of[T]
        val fieldSyms = tpe.typeSymbol.caseFields
        val resolvedElems: List[ToExpr[Any]] = elems.zip(fieldSyms).map:
          case (factory, fieldSym) =>
            tpe.memberType(fieldSym).asType match
              case '[t] => factory.asInstanceOf[ToExprFactory[t]].apply().asInstanceOf[ToExpr[Any]]
        val mirrorExpr = Expr.summon[Mirror.ProductOf[T]].get
        val elemVals = x.asInstanceOf[Product].productIterator.toList
        val elemExprs = resolvedElems.zip(elemVals).map(_.apply(_))
        val tupleExpr = Expr.ofTupleFromSeq(elemExprs)
        '{ $mirrorExpr.fromProduct($tupleExpr) }

  private def derivedSum[T: Mirror.SumOf as m](elemInstances: -> List[ToExprFactory[Any]]): ToExprFactory[T] = new ToExprFactory[T]:
    private lazy val elems = elemInstances
    def apply()(using Type[T]): ToExpr[T] = new ToExpr[T]:
      def apply(x: T)(using quotes: Quotes): Expr[T] =
        import quotes.reflect.*
        val idx = m.ordinal(x)
        val caseSym = TypeRepr.of[T].typeSymbol.children(idx)
        caseSym.typeRef.asType match
          case '[c] => elems(idx).asInstanceOf[ToExprFactory[c]].apply().apply(x.asInstanceOf[c]).asInstanceOf[Expr[T]]

  /** Bridges any type that already has a plain `ToExpr` (e.g. `Int`, `String`) into `ToExprFactory`. */
  given [T] => (te: ToExpr[T]) => ToExprFactory[T]:
    def apply()(using Type[T]): ToExpr[T] = te
