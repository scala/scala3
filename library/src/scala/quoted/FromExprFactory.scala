package scala.quoted

import language.experimental.captureChecking
import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.reflect.ClassTag

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
        val sym = tpe.typeSymbol
        val fieldSyms = sym.caseFields
        val resolvedElems = elems.zip(fieldSyms).map:
          case (factory, fieldSym) =>
            tpe.memberType(fieldSym).asType match
              case '[t] => factory.asInstanceOf[FromExprFactory[t]].apply().asInstanceOf[FromExpr[Any]]

        @tailrec def stripWrappers(t: Term): Term = t match
          case Inlined(_, Nil, e) => stripWrappers(e)
          case Block(Nil, e) => stripWrappers(e)
          case Typed(e, _) => stripWrappers(e)
          case _ => t

        // `new T(args*)` (primary ctor) or `T(args*)` (companion `apply`).
        def isOwnConstructor(fun: Term): Boolean =
          fun.symbol == sym.primaryConstructor
            || (fun.symbol.name == "apply" && fun.symbol.owner == sym.companionModule.moduleClass)

        // defn.TupleClass does not work :/
        def tupleModuleClass =
          Symbol.requiredModule(if resolvedElems.isEmpty then "scala.Tuple" else s"scala.Tuple${elems.size}").moduleClass

        def extractArgs(args: List[Term]) = resolvedElems.zip(args).foldRight(Option(List.empty[Any])):
          case (_, None) => None
          case ((fe, arg), Some(acc)) => fe.unapply(arg.asExprOf[Any]).map(_ :: acc)

        val args = stripWrappers(x.asTerm) match
          case Apply(fun, args) if args.length == resolvedElems.size && isOwnConstructor(fun) =>
            extractArgs(args)
          // case '{ type tuple <: Tuple; { $mirror : Mirror.ProductOf[T] }.fromProduct($tuple : tuple ) } => on x does not work :/
          case Apply(Select(recv, "fromProduct"), List(tupleArg)) if recv.tpe <:< TypeRepr.of[deriving.Mirror.ProductOf[T]] =>
            stripWrappers(tupleArg) match
              case Apply(fun, args)
                if fun.symbol.name == "apply" && fun.symbol.owner == tupleModuleClass && args.length == resolvedElems.size =>
                extractArgs(args)
              case _ => None
          // A bare reference to a singleton (case object/enum case), not a call. TermRef for an explicit given/enum case, TypeRef (via companion) for `derives`.
          case t if resolvedElems.isEmpty && t.symbol == (if tpe.termSymbol.exists then tpe.termSymbol else sym.companionModule) =>
            extractArgs(Nil)
          case _ => None

        args.map(vs => m.fromProduct(Tuple.fromArray(vs.toArray)))

  private def derivedSum[T: Mirror.SumOf](elemInstances: -> List[FromExprFactory[Any]]): FromExprFactory[T] = new FromExprFactory[T]:
    private lazy val elems = elemInstances
    def apply()(using Type[T]): FromExpr[T] = new FromExpr[T]:
      def unapply(x: Expr[T])(using quotes: Quotes): Option[T] =
        import quotes.reflect.*
        val caseSyms = TypeRepr.of[T].typeSymbol.children
        val resolvedElems: List[FromExpr[Any]] = elems.zip(caseSyms).map:
          case (factory, caseSym) =>
            // Parameterless enum cases (`case Red, Green`) are terms, not types; use termRef for those.
            val ref = if caseSym.isTerm then caseSym.termRef else caseSym.typeRef
            ref.asType match
              case '[c] => factory.asInstanceOf[FromExprFactory[c]].apply().asInstanceOf[FromExpr[Any]]
        resolvedElems.iterator.map(_.unapply(x)).collectFirst { case Some(v) => v.asInstanceOf[T] }

  /** Bridges any type that already has a plain `FromExpr` (e.g. `Int`, `String`) into `FromExprFactory`. */
  given fromFromExpr: [T] => (fe: FromExpr[T]) => FromExprFactory[T]:
    def apply()(using Type[T]): FromExpr[T] = fe

  // Container instances below defer `Type[T]` for the same reason as the `ToExprFactory` side.
  // Each decomposes its own `Type[Xxx[T]]` via a quote type pattern to recover `Type[T]`; the
  // cast is safe since the pattern only ever matches with `t =:= T`.

  given arrayFromExprFactory: [T: {FromExprFactory as tf, ClassTag as ct}] => FromExprFactory[Array[T]]:
    def apply()(using Type[Array[T]]): FromExpr[Array[T]] = new FromExpr[Array[T]]:
      def unapply(x: Expr[Array[T]])(using Quotes): Option[Array[T]] = summon[Type[Array[T]]] match
        case '[Array[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          given FromExpr[T] = tf.apply()
          val result: Option[Array[T]] = x match
            case '{ Array[T](${Varargs(Exprs(elems))}*)(using ${_}) } => Some(elems.toArray(using ct))
            case _ => None
          result

  given seqFromExprFactory: [T: FromExprFactory as tf] => FromExprFactory[Seq[T]]:
    def apply()(using Type[Seq[T]]): FromExpr[Seq[T]] = new FromExpr[Seq[T]]:
      def unapply(x: Expr[Seq[T]])(using Quotes): Option[Seq[T]] = summon[Type[Seq[T]]] match
        case '[Seq[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          given FromExpr[T] = tf.apply()
          val result: Option[Seq[T]] = x match
            case Varargs(Exprs(elems)) => Some(elems)
            case '{ scala.Seq[T](${Varargs(Exprs(elems))}*) } => Some(elems)
            case '{ scala.collection.immutable.Seq[T](${Varargs(Exprs(elems))}*) } => Some(elems)
            case '{ ${Expr(xs)}: List[T] } => Some(xs)
            case _ => None
          result

  given listFromExprFactory: [T: FromExprFactory as tf] => FromExprFactory[List[T]]:
    def apply()(using Type[List[T]]): FromExpr[List[T]] = new FromExpr[List[T]]:
      def unapply(x: Expr[List[T]])(using Quotes): Option[List[T]] = summon[Type[List[T]]] match
        case '[List[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          given FromExpr[T] = tf.apply()
          val result: Option[List[T]] = x match
            case '{ scala.List[T](${Varargs(Exprs(elems))}*) } => Some(elems.toList)
            case '{ scala.List.empty[T] } => Some(Nil)
            case '{ Nil } => Some(Nil)
            case '{ scala.collection.immutable.List[T](${Varargs(Exprs(elems))}*) } => Some(elems.toList)
            case '{ scala.collection.immutable.List.empty[T] } => Some(Nil)
            case _ => None
          result

  given setFromExprFactory: [T: FromExprFactory as tf] => FromExprFactory[Set[T]]:
    def apply()(using Type[Set[T]]): FromExpr[Set[T]] = new FromExpr[Set[T]]:
      def unapply(x: Expr[Set[T]])(using Quotes): Option[Set[T]] = summon[Type[Set[T]]] match
        case '[Set[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          given FromExpr[T] = tf.apply()
          val result: Option[Set[T]] = x match
            case '{ Set[T](${Varargs(Exprs(elems))}*) } => Some(elems.toSet)
            case '{ Set.empty[T] } => Some(Set.empty[T])
            case '{ scala.collection.immutable.Set[T](${Varargs(Exprs(elems))}*) } => Some(elems.toSet)
            case '{ scala.collection.immutable.Set.empty[T] } => Some(Set.empty[T])
            case _ => None
          result

  given mapFromExprFactory: [T: FromExprFactory as tf, U: FromExprFactory as uf] => FromExprFactory[Map[T, U]]:
    def apply()(using Type[Map[T, U]]): FromExpr[Map[T, U]] = new FromExpr[Map[T, U]]:
      def unapply(x: Expr[Map[T, U]])(using Quotes): Option[Map[T, U]] = summon[Type[Map[T, U]]] match
        case '[Map[t, u]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          given Type[U] = Type.of[u].asInstanceOf[Type[U]]
          given FromExpr[T] = tf.apply()
          given FromExpr[U] = uf.apply()
          val result: Option[Map[T, U]] = x match
            case '{ Map[T, U](${Varargs(Exprs(elems))}*) } => Some(elems.toMap)
            case '{ Map.empty[T, U] } => Some(Map.empty)
            case '{ scala.collection.immutable.Map[T, U](${Varargs(Exprs(elems))}*) } => Some(elems.toMap)
            case '{ scala.collection.immutable.Map.empty[T, U] } => Some(Map.empty)
            case _ => None
          result

  given optionFromExprFactory: [T: FromExprFactory as tf] => FromExprFactory[Option[T]]:
    def apply()(using Type[Option[T]]): FromExpr[Option[T]] = new FromExpr[Option[T]]:
      def unapply(x: Expr[Option[T]])(using Quotes): Option[Option[T]] = summon[Type[Option[T]]] match
        case '[Option[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          given FromExpr[T] = tf.apply()
          val result: Option[Option[T]] = x match
            case '{ Option[T](${Expr(y)}: T) } => Some(Option(y))
            case '{ None } => Some(None)
            case '{ ${Expr(opt)} : Some[T] } => Some(opt)
            case _ => None
          result

  given someFromExprFactory: [T: FromExprFactory as tf] => FromExprFactory[Some[T]]:
    def apply()(using Type[Some[T]]): FromExpr[Some[T]] = new FromExpr[Some[T]]:
      def unapply(x: Expr[Some[T]])(using Quotes): Option[Some[T]] = summon[Type[Some[T]]] match
        case '[Some[t]] =>
          given Type[T] = Type.of[t].asInstanceOf[Type[T]]
          given FromExpr[T] = tf.apply()
          val result: Option[Some[T]] = x match
            case '{ new Some[T](${Expr(y)}) } => Some(Some(y))
            case '{     Some[T](${Expr(y)}) } => Some(Some(y))
            case _ => None
          result

  given eitherFromExprFactory: [L: FromExprFactory as lf, R: FromExprFactory as rf] => FromExprFactory[Either[L, R]]:
    def apply()(using Type[Either[L, R]]): FromExpr[Either[L, R]] = new FromExpr[Either[L, R]]:
      def unapply(x: Expr[Either[L, R]])(using Quotes): Option[Either[L, R]] = summon[Type[Either[L, R]]] match
        case '[Either[l, r]] =>
          given Type[L] = Type.of[l].asInstanceOf[Type[L]]
          given Type[R] = Type.of[r].asInstanceOf[Type[R]]
          given FromExpr[L] = lf.apply()
          given FromExpr[R] = rf.apply()
          val result: Option[Either[L, R]] = x match
            case '{ $y: Left[L, R] } => y.value
            case '{ $y: Right[L, R] } => y.value
            case _ => None
          result

  given leftFromExprFactory: [L: FromExprFactory as lf, R] => FromExprFactory[Left[L, R]]:
    def apply()(using Type[Left[L, R]]): FromExpr[Left[L, R]] = new FromExpr[Left[L, R]]:
      def unapply(x: Expr[Left[L, R]])(using Quotes): Option[Left[L, R]] = summon[Type[Left[L, R]]] match
        case '[Left[l, r]] =>
          given Type[L] = Type.of[l].asInstanceOf[Type[L]]
          given Type[R] = Type.of[r].asInstanceOf[Type[R]]
          given FromExpr[L] = lf.apply()
          val result: Option[Left[L, R]] = x match
            case '{ Left[L, R](${Expr(y)}) } => Some(Left(y))
            case _ => None
          result

  given rightFromExprFactory: [L, R: FromExprFactory as rf] => FromExprFactory[Right[L, R]]:
    def apply()(using Type[Right[L, R]]): FromExpr[Right[L, R]] = new FromExpr[Right[L, R]]:
      def unapply(x: Expr[Right[L, R]])(using Quotes): Option[Right[L, R]] = summon[Type[Right[L, R]]] match
        case '[Right[l, r]] =>
          given Type[L] = Type.of[l].asInstanceOf[Type[L]]
          given Type[R] = Type.of[r].asInstanceOf[Type[R]]
          given FromExpr[R] = rf.apply()
          val result: Option[Right[L, R]] = x match
            case '{ Right[L, R](${Expr(y)}) } => Some(Right(y))
            case _ => None
          result

  // Tuple instances: same deferred-Type pattern as the containers above, mirroring the
  // hand-written Tuple1..22 instances in FromExpr (there is no hand-written TupleCons FromExpr).

  given tuple1FromExprFactory: [T1: FromExprFactory as tf1] => FromExprFactory[Tuple1[T1]]:
    def apply()(using Type[Tuple1[T1]]): FromExpr[Tuple1[T1]] = new FromExpr[Tuple1[T1]]:
      def unapply(x: Expr[Tuple1[T1]])(using Quotes): Option[Tuple1[T1]] = summon[Type[Tuple1[T1]]] match
        case '[Tuple1[t1]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given FromExpr[T1] = tf1.apply()
          val result: Option[Tuple1[T1]] = x match
            case '{ new Tuple1[T1](${Expr(y1)}) } => Some(Tuple1(y1))
            case '{     Tuple1[T1](${Expr(y1)}) } => Some(Tuple1(y1))
            case _ => None
          result

  given tuple2FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2] => FromExprFactory[Tuple2[T1, T2]]:
    def apply()(using Type[Tuple2[T1, T2]]): FromExpr[Tuple2[T1, T2]] = new FromExpr[Tuple2[T1, T2]]:
      def unapply(x: Expr[Tuple2[T1, T2]])(using Quotes): Option[Tuple2[T1, T2]] = summon[Type[Tuple2[T1, T2]]] match
        case '[Tuple2[t1, t2]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          val result: Option[Tuple2[T1, T2]] = x match
            case '{ new Tuple2[T1, T2](${Expr(y1)}, ${Expr(y2)}) } => Some(Tuple2(y1, y2))
            case '{     Tuple2[T1, T2](${Expr(y1)}, ${Expr(y2)}) } => Some(Tuple2(y1, y2))
            case '{ (${Expr(y1)}: T1) -> (${Expr(y2)}: T2) } => Some(Tuple2(y1, y2))
            case _ => None
          result

  given tuple3FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3] => FromExprFactory[Tuple3[T1, T2, T3]]:
    def apply()(using Type[Tuple3[T1, T2, T3]]): FromExpr[Tuple3[T1, T2, T3]] = new FromExpr[Tuple3[T1, T2, T3]]:
      def unapply(x: Expr[Tuple3[T1, T2, T3]])(using Quotes): Option[Tuple3[T1, T2, T3]] = summon[Type[Tuple3[T1, T2, T3]]] match
        case '[Tuple3[t1, t2, t3]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          val result: Option[Tuple3[T1, T2, T3]] = x match
            case '{ new Tuple3[T1, T2, T3](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}) } => Some(Tuple3(y1, y2, y3))
            case '{     Tuple3[T1, T2, T3](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}) } => Some(Tuple3(y1, y2, y3))
            case _ => None
          result

  given tuple4FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4] => FromExprFactory[Tuple4[T1, T2, T3, T4]]:
    def apply()(using Type[Tuple4[T1, T2, T3, T4]]): FromExpr[Tuple4[T1, T2, T3, T4]] = new FromExpr[Tuple4[T1, T2, T3, T4]]:
      def unapply(x: Expr[Tuple4[T1, T2, T3, T4]])(using Quotes): Option[Tuple4[T1, T2, T3, T4]] = summon[Type[Tuple4[T1, T2, T3, T4]]] match
        case '[Tuple4[t1, t2, t3, t4]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          val result: Option[Tuple4[T1, T2, T3, T4]] = x match
            case '{ new Tuple4[T1, T2, T3, T4](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
            case '{     Tuple4[T1, T2, T3, T4](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}) } => Some(Tuple4(y1, y2, y3, y4))
            case _ => None
          result

  given tuple5FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5] => FromExprFactory[Tuple5[T1, T2, T3, T4, T5]]:
    def apply()(using Type[Tuple5[T1, T2, T3, T4, T5]]): FromExpr[Tuple5[T1, T2, T3, T4, T5]] = new FromExpr[Tuple5[T1, T2, T3, T4, T5]]:
      def unapply(x: Expr[Tuple5[T1, T2, T3, T4, T5]])(using Quotes): Option[Tuple5[T1, T2, T3, T4, T5]] = summon[Type[Tuple5[T1, T2, T3, T4, T5]]] match
        case '[Tuple5[t1, t2, t3, t4, t5]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          val result: Option[Tuple5[T1, T2, T3, T4, T5]] = x match
            case '{ new Tuple5[T1, T2, T3, T4, T5](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
            case '{     Tuple5[T1, T2, T3, T4, T5](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}) } => Some(Tuple5(y1, y2, y3, y4, y5))
            case _ => None
          result

  given tuple6FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6] => FromExprFactory[Tuple6[T1, T2, T3, T4, T5, T6]]:
    def apply()(using Type[Tuple6[T1, T2, T3, T4, T5, T6]]): FromExpr[Tuple6[T1, T2, T3, T4, T5, T6]] = new FromExpr[Tuple6[T1, T2, T3, T4, T5, T6]]:
      def unapply(x: Expr[Tuple6[T1, T2, T3, T4, T5, T6]])(using Quotes): Option[Tuple6[T1, T2, T3, T4, T5, T6]] = summon[Type[Tuple6[T1, T2, T3, T4, T5, T6]]] match
        case '[Tuple6[t1, t2, t3, t4, t5, t6]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          val result: Option[Tuple6[T1, T2, T3, T4, T5, T6]] = x match
            case '{ new Tuple6[T1, T2, T3, T4, T5, T6](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
            case '{     Tuple6[T1, T2, T3, T4, T5, T6](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}) } => Some(Tuple6(y1, y2, y3, y4, y5, y6))
            case _ => None
          result

  given tuple7FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7] => FromExprFactory[Tuple7[T1, T2, T3, T4, T5, T6, T7]]:
    def apply()(using Type[Tuple7[T1, T2, T3, T4, T5, T6, T7]]): FromExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = new FromExpr[Tuple7[T1, T2, T3, T4, T5, T6, T7]]:
      def unapply(x: Expr[Tuple7[T1, T2, T3, T4, T5, T6, T7]])(using Quotes): Option[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = summon[Type[Tuple7[T1, T2, T3, T4, T5, T6, T7]]] match
        case '[Tuple7[t1, t2, t3, t4, t5, t6, t7]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          val result: Option[Tuple7[T1, T2, T3, T4, T5, T6, T7]] = x match
            case '{ new Tuple7[T1, T2, T3, T4, T5, T6, T7](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
            case '{     Tuple7[T1, T2, T3, T4, T5, T6, T7](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}) } => Some(Tuple7(y1, y2, y3, y4, y5, y6, y7))
            case _ => None
          result

  given tuple8FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8] => FromExprFactory[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]:
    def apply()(using Type[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]): FromExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = new FromExpr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]:
      def unapply(x: Expr[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]])(using Quotes): Option[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = summon[Type[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]]] match
        case '[Tuple8[t1, t2, t3, t4, t5, t6, t7, t8]] =>
          given Type[T1] = Type.of[t1].asInstanceOf[Type[T1]]
          given Type[T2] = Type.of[t2].asInstanceOf[Type[T2]]
          given Type[T3] = Type.of[t3].asInstanceOf[Type[T3]]
          given Type[T4] = Type.of[t4].asInstanceOf[Type[T4]]
          given Type[T5] = Type.of[t5].asInstanceOf[Type[T5]]
          given Type[T6] = Type.of[t6].asInstanceOf[Type[T6]]
          given Type[T7] = Type.of[t7].asInstanceOf[Type[T7]]
          given Type[T8] = Type.of[t8].asInstanceOf[Type[T8]]
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          val result: Option[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8]] = x match
            case '{ new Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
            case '{     Tuple8[T1, T2, T3, T4, T5, T6, T7, T8](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}) } => Some(Tuple8(y1, y2, y3, y4, y5, y6, y7, y8))
            case _ => None
          result

  given tuple9FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9] => FromExprFactory[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]:
    def apply()(using Type[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]): FromExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = new FromExpr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]:
      def unapply(x: Expr[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]])(using Quotes): Option[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = summon[Type[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          val result: Option[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]] = x match
            case '{ new Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
            case '{     Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}) } => Some(Tuple9(y1, y2, y3, y4, y5, y6, y7, y8, y9))
            case _ => None
          result

  given tuple10FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10] => FromExprFactory[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]:
    def apply()(using Type[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]): FromExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = new FromExpr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]:
      def unapply(x: Expr[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]])(using Quotes): Option[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = summon[Type[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          val result: Option[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] = x match
            case '{ new Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
            case '{     Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}) } => Some(Tuple10(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10))
            case _ => None
          result

  given tuple11FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11] => FromExprFactory[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]:
    def apply()(using Type[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]): FromExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = new FromExpr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]:
      def unapply(x: Expr[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]])(using Quotes): Option[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = summon[Type[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          val result: Option[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]] = x match
            case '{ new Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
            case '{     Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}) } => Some(Tuple11(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11))
            case _ => None
          result

  given tuple12FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12] => FromExprFactory[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]:
    def apply()(using Type[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]): FromExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = new FromExpr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]:
      def unapply(x: Expr[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]])(using Quotes): Option[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = summon[Type[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          val result: Option[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]] = x match
            case '{ new Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
            case '{     Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}) } => Some(Tuple12(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12))
            case _ => None
          result

  given tuple13FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12, T13: FromExprFactory as tf13] => FromExprFactory[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]:
    def apply()(using Type[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]): FromExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = new FromExpr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]:
      def unapply(x: Expr[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]])(using Quotes): Option[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = summon[Type[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          given FromExpr[T13] = tf13.apply()
          val result: Option[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]] = x match
            case '{ new Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
            case '{     Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}) } => Some(Tuple13(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13))
            case _ => None
          result

  given tuple14FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12, T13: FromExprFactory as tf13, T14: FromExprFactory as tf14] => FromExprFactory[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]:
    def apply()(using Type[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]): FromExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = new FromExpr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]:
      def unapply(x: Expr[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]])(using Quotes): Option[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = summon[Type[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          given FromExpr[T13] = tf13.apply()
          given FromExpr[T14] = tf14.apply()
          val result: Option[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]] = x match
            case '{ new Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
            case '{     Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}) } => Some(Tuple14(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14))
            case _ => None
          result

  given tuple15FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12, T13: FromExprFactory as tf13, T14: FromExprFactory as tf14, T15: FromExprFactory as tf15] => FromExprFactory[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]:
    def apply()(using Type[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]): FromExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = new FromExpr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]:
      def unapply(x: Expr[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]])(using Quotes): Option[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = summon[Type[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          given FromExpr[T13] = tf13.apply()
          given FromExpr[T14] = tf14.apply()
          given FromExpr[T15] = tf15.apply()
          val result: Option[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]] = x match
            case '{ new Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
            case '{     Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}) } => Some(Tuple15(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15))
            case _ => None
          result

  given tuple16FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12, T13: FromExprFactory as tf13, T14: FromExprFactory as tf14, T15: FromExprFactory as tf15, T16: FromExprFactory as tf16] => FromExprFactory[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]:
    def apply()(using Type[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]): FromExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = new FromExpr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]:
      def unapply(x: Expr[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]])(using Quotes): Option[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = summon[Type[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          given FromExpr[T13] = tf13.apply()
          given FromExpr[T14] = tf14.apply()
          given FromExpr[T15] = tf15.apply()
          given FromExpr[T16] = tf16.apply()
          val result: Option[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]] = x match
            case '{ new Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
            case '{     Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}) } => Some(Tuple16(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16))
            case _ => None
          result

  given tuple17FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12, T13: FromExprFactory as tf13, T14: FromExprFactory as tf14, T15: FromExprFactory as tf15, T16: FromExprFactory as tf16, T17: FromExprFactory as tf17] => FromExprFactory[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]:
    def apply()(using Type[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]): FromExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = new FromExpr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]:
      def unapply(x: Expr[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]])(using Quotes): Option[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = summon[Type[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          given FromExpr[T13] = tf13.apply()
          given FromExpr[T14] = tf14.apply()
          given FromExpr[T15] = tf15.apply()
          given FromExpr[T16] = tf16.apply()
          given FromExpr[T17] = tf17.apply()
          val result: Option[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]] = x match
            case '{ new Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
            case '{     Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}) } => Some(Tuple17(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17))
            case _ => None
          result

  given tuple18FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12, T13: FromExprFactory as tf13, T14: FromExprFactory as tf14, T15: FromExprFactory as tf15, T16: FromExprFactory as tf16, T17: FromExprFactory as tf17, T18: FromExprFactory as tf18] => FromExprFactory[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]:
    def apply()(using Type[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]): FromExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = new FromExpr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]:
      def unapply(x: Expr[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]])(using Quotes): Option[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = summon[Type[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          given FromExpr[T13] = tf13.apply()
          given FromExpr[T14] = tf14.apply()
          given FromExpr[T15] = tf15.apply()
          given FromExpr[T16] = tf16.apply()
          given FromExpr[T17] = tf17.apply()
          given FromExpr[T18] = tf18.apply()
          val result: Option[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]] = x match
            case '{ new Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
            case '{     Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}) } => Some(Tuple18(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18))
            case _ => None
          result

  given tuple19FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12, T13: FromExprFactory as tf13, T14: FromExprFactory as tf14, T15: FromExprFactory as tf15, T16: FromExprFactory as tf16, T17: FromExprFactory as tf17, T18: FromExprFactory as tf18, T19: FromExprFactory as tf19] => FromExprFactory[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]:
    def apply()(using Type[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]): FromExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = new FromExpr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]:
      def unapply(x: Expr[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]])(using Quotes): Option[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = summon[Type[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          given FromExpr[T13] = tf13.apply()
          given FromExpr[T14] = tf14.apply()
          given FromExpr[T15] = tf15.apply()
          given FromExpr[T16] = tf16.apply()
          given FromExpr[T17] = tf17.apply()
          given FromExpr[T18] = tf18.apply()
          given FromExpr[T19] = tf19.apply()
          val result: Option[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]] = x match
            case '{ new Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
            case '{     Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}) } => Some(Tuple19(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19))
            case _ => None
          result

  given tuple20FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12, T13: FromExprFactory as tf13, T14: FromExprFactory as tf14, T15: FromExprFactory as tf15, T16: FromExprFactory as tf16, T17: FromExprFactory as tf17, T18: FromExprFactory as tf18, T19: FromExprFactory as tf19, T20: FromExprFactory as tf20] => FromExprFactory[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]:
    def apply()(using Type[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]): FromExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = new FromExpr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]:
      def unapply(x: Expr[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]])(using Quotes): Option[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = summon[Type[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          given FromExpr[T13] = tf13.apply()
          given FromExpr[T14] = tf14.apply()
          given FromExpr[T15] = tf15.apply()
          given FromExpr[T16] = tf16.apply()
          given FromExpr[T17] = tf17.apply()
          given FromExpr[T18] = tf18.apply()
          given FromExpr[T19] = tf19.apply()
          given FromExpr[T20] = tf20.apply()
          val result: Option[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]] = x match
            case '{ new Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
            case '{     Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}) } => Some(Tuple20(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20))
            case _ => None
          result

  given tuple21FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12, T13: FromExprFactory as tf13, T14: FromExprFactory as tf14, T15: FromExprFactory as tf15, T16: FromExprFactory as tf16, T17: FromExprFactory as tf17, T18: FromExprFactory as tf18, T19: FromExprFactory as tf19, T20: FromExprFactory as tf20, T21: FromExprFactory as tf21] => FromExprFactory[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]:
    def apply()(using Type[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]): FromExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = new FromExpr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]:
      def unapply(x: Expr[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]])(using Quotes): Option[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = summon[Type[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          given FromExpr[T13] = tf13.apply()
          given FromExpr[T14] = tf14.apply()
          given FromExpr[T15] = tf15.apply()
          given FromExpr[T16] = tf16.apply()
          given FromExpr[T17] = tf17.apply()
          given FromExpr[T18] = tf18.apply()
          given FromExpr[T19] = tf19.apply()
          given FromExpr[T20] = tf20.apply()
          given FromExpr[T21] = tf21.apply()
          val result: Option[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] = x match
            case '{ new Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
            case '{     Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}) } => Some(Tuple21(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21))
            case _ => None
          result

  given tuple22FromExprFactory: [T1: FromExprFactory as tf1, T2: FromExprFactory as tf2, T3: FromExprFactory as tf3, T4: FromExprFactory as tf4, T5: FromExprFactory as tf5, T6: FromExprFactory as tf6, T7: FromExprFactory as tf7, T8: FromExprFactory as tf8, T9: FromExprFactory as tf9, T10: FromExprFactory as tf10, T11: FromExprFactory as tf11, T12: FromExprFactory as tf12, T13: FromExprFactory as tf13, T14: FromExprFactory as tf14, T15: FromExprFactory as tf15, T16: FromExprFactory as tf16, T17: FromExprFactory as tf17, T18: FromExprFactory as tf18, T19: FromExprFactory as tf19, T20: FromExprFactory as tf20, T21: FromExprFactory as tf21, T22: FromExprFactory as tf22] => FromExprFactory[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]:
    def apply()(using Type[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]): FromExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = new FromExpr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]:
      def unapply(x: Expr[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]])(using Quotes): Option[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = summon[Type[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]] match
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
          given FromExpr[T1] = tf1.apply()
          given FromExpr[T2] = tf2.apply()
          given FromExpr[T3] = tf3.apply()
          given FromExpr[T4] = tf4.apply()
          given FromExpr[T5] = tf5.apply()
          given FromExpr[T6] = tf6.apply()
          given FromExpr[T7] = tf7.apply()
          given FromExpr[T8] = tf8.apply()
          given FromExpr[T9] = tf9.apply()
          given FromExpr[T10] = tf10.apply()
          given FromExpr[T11] = tf11.apply()
          given FromExpr[T12] = tf12.apply()
          given FromExpr[T13] = tf13.apply()
          given FromExpr[T14] = tf14.apply()
          given FromExpr[T15] = tf15.apply()
          given FromExpr[T16] = tf16.apply()
          given FromExpr[T17] = tf17.apply()
          given FromExpr[T18] = tf18.apply()
          given FromExpr[T19] = tf19.apply()
          given FromExpr[T20] = tf20.apply()
          given FromExpr[T21] = tf21.apply()
          given FromExpr[T22] = tf22.apply()
          val result: Option[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]] = x match
            case '{ new Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}, ${Expr(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
            case '{     Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](${Expr(y1)}, ${Expr(y2)}, ${Expr(y3)}, ${Expr(y4)}, ${Expr(y5)}, ${Expr(y6)}, ${Expr(y7)}, ${Expr(y8)}, ${Expr(y9)}, ${Expr(y10)}, ${Expr(y11)}, ${Expr(y12)}, ${Expr(y13)}, ${Expr(y14)}, ${Expr(y15)}, ${Expr(y16)}, ${Expr(y17)}, ${Expr(y18)}, ${Expr(y19)}, ${Expr(y20)}, ${Expr(y21)}, ${Expr(y22)}) } => Some(Tuple22(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14, y15, y16, y17, y18, y19, y20, y21, y22))
            case _ => None
          result
