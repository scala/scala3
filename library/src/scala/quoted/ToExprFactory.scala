package scala.quoted

import language.experimental.captureChecking
import scala.deriving.Mirror
import scala.reflect.ClassTag

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
        // Parameterless enum cases (`case Red, Green`) are terms, not types; use termRef for those.
        val ref = if caseSym.isTerm then caseSym.termRef else caseSym.typeRef
        ref.asType match
          case '[c] => elems(idx).asInstanceOf[ToExprFactory[c]].apply().apply(x.asInstanceOf[c]).asInstanceOf[Expr[T]]

  /** Bridges any type that already has a plain `ToExpr` (e.g. `Int`, `String`) into `ToExprFactory`. */
  given fromToExpr: [T] => (te: ToExpr[T]) => ToExprFactory[T]:
    def apply()(using Type[T]): ToExpr[T] = te

  // Container instances below defer `Type[T]` the same way `derivedProduct`/`derivedSum` do:
  // they only need a `ToExprFactory` for the element type(s), not a `Type` for them, so they
  // stay usable as fields of a `derives ToExprFactory` type without forcing `(using Quotes)`
  // on the enclosing given. Each decomposes its own `Type[Xxx[T]]` via a quote type pattern to
  // recover `Type[T]`; the cast is safe since the pattern only ever matches with `t =:= T`.

  given classTagToExprFactory: [T] => ToExprFactory[ClassTag[T]]:
    def apply()(using Type[ClassTag[T]]): ToExpr[ClassTag[T]] = new ToExpr[ClassTag[T]]:
      def apply(ct: ClassTag[T])(using Quotes): Expr[ClassTag[T]] = summon[Type[ClassTag[T]]] match
        case '[ClassTag[t]] =>
          '{ ClassTag[t](${Expr(ct.runtimeClass.asInstanceOf[Class[t]])}) }.asInstanceOf[Expr[ClassTag[T]]]

  given arrayToExprFactory: [T: {ToExprFactory, ClassTag as ct}] => ToExprFactory[Array[T]]:
    def apply()(using Type[Array[T]]): ToExpr[Array[T]] = new ToExpr[Array[T]]:
      def apply(arr: Array[T])(using Quotes): Expr[Array[T]] = summon[Type[Array[T]]] match
        case '[Array[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          val elemExprs = Expr.ofSeq(arr.toSeq.map(Expr(_)))
          '{ Array[T]($elemExprs*)(using ${Expr(ct)}) }

  given seqToExprFactory: [T: ToExprFactory] => ToExprFactory[Seq[T]]:
    def apply()(using Type[Seq[T]]): ToExpr[Seq[T]] = new ToExpr[Seq[T]]:
      def apply(xs: Seq[T])(using Quotes): Expr[Seq[T]] = summon[Type[Seq[T]]] match
        case '[Seq[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          Expr.ofSeq(xs.map(Expr(_)))

  given listToExprFactory: [T: ToExprFactory] => ToExprFactory[List[T]]:
    def apply()(using Type[List[T]]): ToExpr[List[T]] = new ToExpr[List[T]]:
      def apply(xs: List[T])(using Quotes): Expr[List[T]] = summon[Type[List[T]]] match
        case '[List[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          Expr.ofList(xs.map(Expr(_)))

  given setToExprFactory: [T: ToExprFactory] => ToExprFactory[Set[T]]:
    def apply()(using Type[Set[T]]): ToExpr[Set[T]] = new ToExpr[Set[T]]:
      def apply(xs: Set[T])(using Quotes): Expr[Set[T]] = summon[Type[Set[T]]] match
        case '[Set[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          val elemExprs = Expr.ofSeq(xs.toSeq.map(Expr(_)))
          '{ Set($elemExprs*) }

  given mapToExprFactory: [T: ToExprFactory, U: ToExprFactory] => ToExprFactory[Map[T, U]]:
    def apply()(using Type[Map[T, U]]): ToExpr[Map[T, U]] = new ToExpr[Map[T, U]]:
      def apply(m: Map[T, U])(using Quotes): Expr[Map[T, U]] = summon[Type[Map[T, U]]] match
        case '[Map[t, u]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          given Type[U] = Type.of[u].asInstanceOf[Type[U]]
          val pairs = m.toSeq.map((k, v) => '{ (${Expr(k)}, ${Expr(v)}) })
          '{ Map(${Expr.ofSeq(pairs)}*) }

  given optionToExprFactory: [T: ToExprFactory] => ToExprFactory[Option[T]]:
    def apply()(using Type[Option[T]]): ToExpr[Option[T]] = new ToExpr[Option[T]]:
      def apply(x: Option[T])(using Quotes): Expr[Option[T]] = summon[Type[Option[T]]] match
        case '[Option[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          x match
            case Some(v) => '{ Some[T](${Expr(v)}) }
            case None => '{ None }

  given someToExprFactory: [T: ToExprFactory] => ToExprFactory[Some[T]]:
    def apply()(using Type[Some[T]]): ToExpr[Some[T]] = new ToExpr[Some[T]]:
      def apply(x: Some[T])(using Quotes): Expr[Some[T]] = summon[Type[Some[T]]] match
        case '[Some[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          '{ Some[T](${Expr(x.get)}) }

  given eitherToExprFactory: [L: ToExprFactory, R: ToExprFactory] => ToExprFactory[Either[L, R]]:
    def apply()(using Type[Either[L, R]]): ToExpr[Either[L, R]] = new ToExpr[Either[L, R]]:
      def apply(x: Either[L, R])(using Quotes): Expr[Either[L, R]] = summon[Type[Either[L, R]]] match
        case '[Either[l, r]] =>
          given Type[L] = Type.of[l].asInstanceOf[Type[L]]
          given Type[R] = Type.of[r].asInstanceOf[Type[R]]
          x match
            case scala.util.Left(v) => '{ Left[L, R](${Expr(v)}) }
            case scala.util.Right(v) => '{ Right[L, R](${Expr(v)}) }

  given leftToExprFactory: [L: ToExprFactory, R] => ToExprFactory[Left[L, R]]:
    def apply()(using Type[Left[L, R]]): ToExpr[Left[L, R]] = new ToExpr[Left[L, R]]:
      def apply(x: Left[L, R])(using Quotes): Expr[Left[L, R]] = summon[Type[Left[L, R]]] match
        case '[Left[l, r]] =>
          given Type[L] = Type.of[l].asInstanceOf[Type[L]]
          given Type[R] = Type.of[r].asInstanceOf[Type[R]]
          '{ Left[L, R](${Expr(x.value)}) }

  given rightToExprFactory: [L, R: ToExprFactory] => ToExprFactory[Right[L, R]]:
    def apply()(using Type[Right[L, R]]): ToExpr[Right[L, R]] = new ToExpr[Right[L, R]]:
      def apply(x: Right[L, R])(using Quotes): Expr[Right[L, R]] = summon[Type[Right[L, R]]] match
        case '[Right[l, r]] =>
          given Type[L] = Type.of[l].asInstanceOf[Type[L]]
          given Type[R] = Type.of[r].asInstanceOf[Type[R]]
          '{ Right[L, R](${Expr(x.value)}) }

  // Tuple instances: same deferred-Type pattern as the containers above, mirroring the
  // hand-written Tuple1..22/TupleCons instances in ToExprFactory/FromExprFactory.

  given tuple1ToExprFactory: [T1: ToExprFactory] => ToExprFactory[Tuple1[T1]]:
    def apply()(using Type[Tuple1[T1]]): ToExpr[Tuple1[T1]] = new ToExpr[Tuple1[T1]]:
      def apply(tup: Tuple1[T1])(using Quotes): Expr[Tuple1[T1]] = summon[Type[Tuple1[T1]]] match
        case '[Tuple1[t1]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          '{ Tuple1(${Expr(tup._1)}) }


  given tuple2ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory] => ToExprFactory[Tuple2[T1, T2]]:
    def apply()(using Type[Tuple2[T1, T2]]): ToExpr[Tuple2[T1, T2]] = new ToExpr[Tuple2[T1, T2]]:
      def apply(tup: Tuple2[T1, T2])(using Quotes): Expr[Tuple2[T1, T2]] = summon[Type[Tuple2[T1, T2]]] match
        case '[Tuple2[t1, t2]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}) }


  given tuple3ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory] => ToExprFactory[Tuple3[T1, T2, T3]]:
    def apply()(using Type[Tuple3[T1, T2, T3]]): ToExpr[Tuple3[T1, T2, T3]] = new ToExpr[Tuple3[T1, T2, T3]]:
      def apply(tup: Tuple3[T1, T2, T3])(using Quotes): Expr[Tuple3[T1, T2, T3]] = summon[Type[Tuple3[T1, T2, T3]]] match
        case '[Tuple3[t1, t2, t3]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}) }


  given tuple4ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory] => ToExprFactory[Tuple4[T1, T2, T3, T4]]:
    def apply()(using Type[Tuple4[T1, T2, T3, T4]]): ToExpr[Tuple4[T1, T2, T3, T4]] = new ToExpr[Tuple4[T1, T2, T3, T4]]:
      def apply(tup: Tuple4[T1, T2, T3, T4])(using Quotes): Expr[Tuple4[T1, T2, T3, T4]] = summon[Type[Tuple4[T1, T2, T3, T4]]] match
        case '[Tuple4[t1, t2, t3, t4]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}) }


  given tuple5ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory] => ToExprFactory[Tuple5[T1, T2, T3, T4, T5]]:
    def apply()(using Type[Tuple5[T1, T2, T3, T4, T5]]): ToExpr[Tuple5[T1, T2, T3, T4, T5]] = new ToExpr[Tuple5[T1, T2, T3, T4, T5]]:
      def apply(tup: Tuple5[T1, T2, T3, T4, T5])(using Quotes): Expr[Tuple5[T1, T2, T3, T4, T5]] = summon[Type[Tuple5[T1, T2, T3, T4, T5]]] match
        case '[Tuple5[t1, t2, t3, t4, t5]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}) }


  given tuple6ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory] => ToExprFactory[Tuple6[T1, T2, T3, T4, T5, T6]]:
    def apply()(using Type[Tuple6[T1, T2, T3, T4, T5, T6]]): ToExpr[Tuple6[T1, T2, T3, T4, T5, T6]] = new ToExpr[Tuple6[T1, T2, T3, T4, T5, T6]]:
      def apply(tup: Tuple6[T1, T2, T3, T4, T5, T6])(using Quotes): Expr[Tuple6[T1, T2, T3, T4, T5, T6]] = summon[Type[Tuple6[T1, T2, T3, T4, T5, T6]]] match
        case '[Tuple6[t1, t2, t3, t4, t5, t6]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}) }


  given tuple7ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory] => ToExprFactory[Tuple7[T1, T2, T3, T4, T5, T6, T7]]:
    def apply()(using Type[Tuple7[T1, T2, T3, T4, T5, T6, T7]]): ToExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = new ToExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]:
      def apply(tup: Tuple7[T1, T2, T3, T4, T5, T6, T7])(using Quotes): Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = summon[Type[Tuple7[T1, T2, T3, T4, T5, T6, T7]]] match
        case '[Tuple7[t1, t2, t3, t4, t5, t6, t7]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}) }


  given tuple8ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory] => ToExprFactory[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]:
    def apply()(using Type[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]): ToExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = new ToExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]:
      def apply(tup: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8])(using Quotes): Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = summon[Type[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]] match
        case '[Tuple8[t1, t2, t3, t4, t5, t6, t7, t8]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}) }


  given tuple9ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory] => ToExprFactory[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]:
    def apply()(using Type[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]): ToExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new ToExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]:
      def apply(tup: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9])(using Quotes): Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = summon[Type[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]] match
        case '[Tuple9[t1, t2, t3, t4, t5, t6, t7, t8, t9]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}) }


  given tuple10ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory] => ToExprFactory[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]:
    def apply()(using Type[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]): ToExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = new ToExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]:
      def apply(tup: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10])(using Quotes): Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = summon[Type[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]] match
        case '[Tuple10[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}) }


  given tuple11ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory] => ToExprFactory[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]:
    def apply()(using Type[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]): ToExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = new ToExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]:
      def apply(tup: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11])(using Quotes): Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = summon[Type[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]] match
        case '[Tuple11[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}) }


  given tuple12ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory] => ToExprFactory[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]:
    def apply()(using Type[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]): ToExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = new ToExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]:
      def apply(tup: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12])(using Quotes): Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = summon[Type[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]] match
        case '[Tuple12[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}) }


  given tuple13ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory, T13: ToExprFactory] => ToExprFactory[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]:
    def apply()(using Type[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]): ToExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = new ToExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]:
      def apply(tup: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13])(using Quotes): Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = summon[Type[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]] match
        case '[Tuple13[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          given Type[T13] = Type.of[t13].asInstanceOf[Type[T13]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}, ${Expr(tup._13)}) }


  given tuple14ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory, T13: ToExprFactory, T14: ToExprFactory] => ToExprFactory[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]:
    def apply()(using Type[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]): ToExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = new ToExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]:
      def apply(tup: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14])(using Quotes): Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = summon[Type[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]] match
        case '[Tuple14[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          given Type[T13] = Type.of[t13].asInstanceOf[Type[T13]]
          given Type[T14] = Type.of[t14].asInstanceOf[Type[T14]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}, ${Expr(tup._13)}, ${Expr(tup._14)}) }


  given tuple15ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory, T13: ToExprFactory, T14: ToExprFactory, T15: ToExprFactory] => ToExprFactory[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]:
    def apply()(using Type[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]): ToExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = new ToExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]:
      def apply(tup: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15])(using Quotes): Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = summon[Type[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]] match
        case '[Tuple15[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          given Type[T13] = Type.of[t13].asInstanceOf[Type[T13]]
          given Type[T14] = Type.of[t14].asInstanceOf[Type[T14]]
          given Type[T15] = Type.of[t15].asInstanceOf[Type[T15]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}, ${Expr(tup._13)}, ${Expr(tup._14)}, ${Expr(tup._15)}) }


  given tuple16ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory, T13: ToExprFactory, T14: ToExprFactory, T15: ToExprFactory, T16: ToExprFactory] => ToExprFactory[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]:
    def apply()(using Type[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]): ToExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = new ToExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]:
      def apply(tup: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16])(using Quotes): Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = summon[Type[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]] match
        case '[Tuple16[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          given Type[T13] = Type.of[t13].asInstanceOf[Type[T13]]
          given Type[T14] = Type.of[t14].asInstanceOf[Type[T14]]
          given Type[T15] = Type.of[t15].asInstanceOf[Type[T15]]
          given Type[T16] = Type.of[t16].asInstanceOf[Type[T16]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}, ${Expr(tup._13)}, ${Expr(tup._14)}, ${Expr(tup._15)}, ${Expr(tup._16)}) }


  given tuple17ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory, T13: ToExprFactory, T14: ToExprFactory, T15: ToExprFactory, T16: ToExprFactory, T17: ToExprFactory] => ToExprFactory[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]:
    def apply()(using Type[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]): ToExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = new ToExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]:
      def apply(tup: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17])(using Quotes): Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = summon[Type[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]] match
        case '[Tuple17[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          given Type[T13] = Type.of[t13].asInstanceOf[Type[T13]]
          given Type[T14] = Type.of[t14].asInstanceOf[Type[T14]]
          given Type[T15] = Type.of[t15].asInstanceOf[Type[T15]]
          given Type[T16] = Type.of[t16].asInstanceOf[Type[T16]]
          given Type[T17] = Type.of[t17].asInstanceOf[Type[T17]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}, ${Expr(tup._13)}, ${Expr(tup._14)}, ${Expr(tup._15)}, ${Expr(tup._16)}, ${Expr(tup._17)}) }


  given tuple18ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory, T13: ToExprFactory, T14: ToExprFactory, T15: ToExprFactory, T16: ToExprFactory, T17: ToExprFactory, T18: ToExprFactory] => ToExprFactory[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]:
    def apply()(using Type[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]): ToExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = new ToExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]:
      def apply(tup: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18])(using Quotes): Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = summon[Type[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]] match
        case '[Tuple18[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          given Type[T13] = Type.of[t13].asInstanceOf[Type[T13]]
          given Type[T14] = Type.of[t14].asInstanceOf[Type[T14]]
          given Type[T15] = Type.of[t15].asInstanceOf[Type[T15]]
          given Type[T16] = Type.of[t16].asInstanceOf[Type[T16]]
          given Type[T17] = Type.of[t17].asInstanceOf[Type[T17]]
          given Type[T18] = Type.of[t18].asInstanceOf[Type[T18]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}, ${Expr(tup._13)}, ${Expr(tup._14)}, ${Expr(tup._15)}, ${Expr(tup._16)}, ${Expr(tup._17)}, ${Expr(tup._18)}) }


  given tuple19ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory, T13: ToExprFactory, T14: ToExprFactory, T15: ToExprFactory, T16: ToExprFactory, T17: ToExprFactory, T18: ToExprFactory, T19: ToExprFactory] => ToExprFactory[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]:
    def apply()(using Type[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]): ToExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = new ToExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]:
      def apply(tup: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19])(using Quotes): Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = summon[Type[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]] match
        case '[Tuple19[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          given Type[T13] = Type.of[t13].asInstanceOf[Type[T13]]
          given Type[T14] = Type.of[t14].asInstanceOf[Type[T14]]
          given Type[T15] = Type.of[t15].asInstanceOf[Type[T15]]
          given Type[T16] = Type.of[t16].asInstanceOf[Type[T16]]
          given Type[T17] = Type.of[t17].asInstanceOf[Type[T17]]
          given Type[T18] = Type.of[t18].asInstanceOf[Type[T18]]
          given Type[T19] = Type.of[t19].asInstanceOf[Type[T19]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}, ${Expr(tup._13)}, ${Expr(tup._14)}, ${Expr(tup._15)}, ${Expr(tup._16)}, ${Expr(tup._17)}, ${Expr(tup._18)}, ${Expr(tup._19)}) }


  given tuple20ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory, T13: ToExprFactory, T14: ToExprFactory, T15: ToExprFactory, T16: ToExprFactory, T17: ToExprFactory, T18: ToExprFactory, T19: ToExprFactory, T20: ToExprFactory] => ToExprFactory[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]:
    def apply()(using Type[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]): ToExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = new ToExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]:
      def apply(tup: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20])(using Quotes): Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = summon[Type[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]] match
        case '[Tuple20[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          given Type[T13] = Type.of[t13].asInstanceOf[Type[T13]]
          given Type[T14] = Type.of[t14].asInstanceOf[Type[T14]]
          given Type[T15] = Type.of[t15].asInstanceOf[Type[T15]]
          given Type[T16] = Type.of[t16].asInstanceOf[Type[T16]]
          given Type[T17] = Type.of[t17].asInstanceOf[Type[T17]]
          given Type[T18] = Type.of[t18].asInstanceOf[Type[T18]]
          given Type[T19] = Type.of[t19].asInstanceOf[Type[T19]]
          given Type[T20] = Type.of[t20].asInstanceOf[Type[T20]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}, ${Expr(tup._13)}, ${Expr(tup._14)}, ${Expr(tup._15)}, ${Expr(tup._16)}, ${Expr(tup._17)}, ${Expr(tup._18)}, ${Expr(tup._19)}, ${Expr(tup._20)}) }


  given tuple21ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory, T13: ToExprFactory, T14: ToExprFactory, T15: ToExprFactory, T16: ToExprFactory, T17: ToExprFactory, T18: ToExprFactory, T19: ToExprFactory, T20: ToExprFactory, T21: ToExprFactory] => ToExprFactory[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]:
    def apply()(using Type[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]): ToExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = new ToExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]:
      def apply(tup: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21])(using Quotes): Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = summon[Type[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]] match
        case '[Tuple21[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          given Type[T13] = Type.of[t13].asInstanceOf[Type[T13]]
          given Type[T14] = Type.of[t14].asInstanceOf[Type[T14]]
          given Type[T15] = Type.of[t15].asInstanceOf[Type[T15]]
          given Type[T16] = Type.of[t16].asInstanceOf[Type[T16]]
          given Type[T17] = Type.of[t17].asInstanceOf[Type[T17]]
          given Type[T18] = Type.of[t18].asInstanceOf[Type[T18]]
          given Type[T19] = Type.of[t19].asInstanceOf[Type[T19]]
          given Type[T20] = Type.of[t20].asInstanceOf[Type[T20]]
          given Type[T21] = Type.of[t21].asInstanceOf[Type[T21]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}, ${Expr(tup._13)}, ${Expr(tup._14)}, ${Expr(tup._15)}, ${Expr(tup._16)}, ${Expr(tup._17)}, ${Expr(tup._18)}, ${Expr(tup._19)}, ${Expr(tup._20)}, ${Expr(tup._21)}) }


  given tuple22ToExprFactory: [T1: ToExprFactory, T2: ToExprFactory, T3: ToExprFactory, T4: ToExprFactory, T5: ToExprFactory, T6: ToExprFactory, T7: ToExprFactory, T8: ToExprFactory, T9: ToExprFactory, T10: ToExprFactory, T11: ToExprFactory, T12: ToExprFactory, T13: ToExprFactory, T14: ToExprFactory, T15: ToExprFactory, T16: ToExprFactory, T17: ToExprFactory, T18: ToExprFactory, T19: ToExprFactory, T20: ToExprFactory, T21: ToExprFactory, T22: ToExprFactory] => ToExprFactory[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]:
    def apply()(using Type[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]): ToExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = new ToExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]:
      def apply(tup: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22])(using Quotes): Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = summon[Type[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]] match
        case '[Tuple22[t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given Type[T9] = Type.of[t9].asInstanceOf[Type[T9]]
          given Type[T10] = Type.of[t10].asInstanceOf[Type[T10]]
          given Type[T11] = Type.of[t11].asInstanceOf[Type[T11]]
          given Type[T12] = Type.of[t12].asInstanceOf[Type[T12]]
          given Type[T13] = Type.of[t13].asInstanceOf[Type[T13]]
          given Type[T14] = Type.of[t14].asInstanceOf[Type[T14]]
          given Type[T15] = Type.of[t15].asInstanceOf[Type[T15]]
          given Type[T16] = Type.of[t16].asInstanceOf[Type[T16]]
          given Type[T17] = Type.of[t17].asInstanceOf[Type[T17]]
          given Type[T18] = Type.of[t18].asInstanceOf[Type[T18]]
          given Type[T19] = Type.of[t19].asInstanceOf[Type[T19]]
          given Type[T20] = Type.of[t20].asInstanceOf[Type[T20]]
          given Type[T21] = Type.of[t21].asInstanceOf[Type[T21]]
          given Type[T22] = Type.of[t22].asInstanceOf[Type[T22]]
          '{ (${Expr(tup._1)}, ${Expr(tup._2)}, ${Expr(tup._3)}, ${Expr(tup._4)}, ${Expr(tup._5)}, ${Expr(tup._6)}, ${Expr(tup._7)}, ${Expr(tup._8)}, ${Expr(tup._9)}, ${Expr(tup._10)}, ${Expr(tup._11)}, ${Expr(tup._12)}, ${Expr(tup._13)}, ${Expr(tup._14)}, ${Expr(tup._15)}, ${Expr(tup._16)}, ${Expr(tup._17)}, ${Expr(tup._18)}, ${Expr(tup._19)}, ${Expr(tup._20)}, ${Expr(tup._21)}, ${Expr(tup._22)}) }


  given tupleConsToExprFactory: [H: ToExprFactory, T <: Tuple: ToExprFactory] => ToExprFactory[H *: T]:
    def apply()(using Type[H *: T]): ToExpr[H *: T] = new ToExpr[H *: T]:
      def apply(tup: H *: T)(using Quotes): Expr[H *: T] = summon[Type[H *: T]] match
        case '[h *: t] =>
          given Type[H] = Type.of[h].asInstanceOf[Type[H]]
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          val head = Expr(tup.head)
          val tail = Expr(tup.tail)
          '{ $head *: $tail }
