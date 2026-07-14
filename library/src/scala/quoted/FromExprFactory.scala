package scala.quoted

import language.experimental.captureChecking
import scala.annotation.tailrec
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
            caseSym.typeRef.asType match
              case '[c] => factory.asInstanceOf[FromExprFactory[c]].apply().asInstanceOf[FromExpr[Any]]
        resolvedElems.iterator.map(_.unapply(x)).collectFirst { case Some(v) => v.asInstanceOf[T] }

  /** Bridges any type that already has a plain `FromExpr` (e.g. `Int`, `String`) into `FromExprFactory`. */
  given [T] => (fe: FromExpr[T]) => FromExprFactory[T]:
    def apply()(using Type[T]): FromExpr[T] = fe
