package derivation
import scala.quoted.*

import scala.annotation.tailrec

object Helpers:

  // file a.scala
  inline def summonAllOptimized[T <: Tuple]: T =
    ${ summonAllOptimizedImpl[T] }

  inline def summon23[E]: (E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E) =
    ${ summon23Impl[E] }

  private def summonAllOptimizedImpl[T <: Tuple: Type](using q: Quotes): Expr[T] = {
    import q.reflect.*

    Expr
      .ofTupleFromSeq(typesOfTuple(TypeRepr.of[T], Nil).map { tpe =>
        tpe.asType match {
          case '[t] =>
            Expr.summon[t].getOrElse(report.errorAndAbort(s"Unable to to find implicit instance for ${tpe.show}"))
        }
      })
      .asExprOf[T]
  }

  private def summon23Impl[E: Type](using q: Quotes): Expr[(E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E, E)] = {
    import q.reflect.*

    val e = Expr.summon[E].getOrElse(report.errorAndAbort(s"Unable to to find implicit instance for ${TypeRepr.of[E].show}"))

    val tuple = (e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e, e)

    assert(tuple.size == 23)

    Expr.ofTuple(tuple)
  }

  @tailrec
  private[derivation] def typesOfTuple(
      using q: Quotes
  )(tpe: q.reflect.TypeRepr, acc: List[q.reflect.TypeRepr]): List[q.reflect.TypeRepr] =
    import q.reflect.*
    val cons = Symbol.classSymbol("scala.*:")
    tpe.widenTermRefByName.dealias match
      case AppliedType(fn, tpes) if defn.isTupleClass(fn.typeSymbol) =>
        tpes.reverse_:::(acc)
      case AppliedType(tp, List(headType, tailType)) if tp.derivesFrom(cons) =>
        typesOfTuple(tailType, headType :: acc)
      case tpe =>
        if tpe.derivesFrom(Symbol.classSymbol("scala.EmptyTuple")) then acc.reverse
        else report.errorAndAbort(s"Unknown type encountered in tuple ${tpe.show}")
