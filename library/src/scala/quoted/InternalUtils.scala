package scala.quoted

import scala.annotation.tailrec
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

private[quoted] def unapplyProduct[T: {Type, Mirror.ProductOf as m}](using quotes: Quotes)(elems: List[FromExpr[Any]])(x: Expr[T]): Option[T] =
  import quotes.reflect.*
  val tpe = TypeRepr.of[T]
  val sym = tpe.typeSymbol

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
    Symbol.requiredModule(if elems.isEmpty then "scala.Tuple" else s"scala.Tuple${elems.size}").moduleClass

  def extractArgs(args: List[Term]) = elems.zip(args).foldRight(Option(List.empty[Any])):
    case (_, None) => None
    case ((fe, arg), Some(acc)) => fe.unapply(arg.asExprOf[Any]).map(_ :: acc)

  val args = stripWrappers(x.asTerm) match
    case Apply(fun, args) if args.length == elems.size && isOwnConstructor(fun) =>
      extractArgs(args)
    // case '{ type tuple <: Tuple; { $mirror : Mirror.ProductOf[T] }.fromProduct($tuple : tuple ) } => on x does not work :/
    case Apply(Select(recv, "fromProduct"), List(tupleArg)) if recv.tpe <:< TypeRepr.of[deriving.Mirror.ProductOf[T]] =>
      stripWrappers(tupleArg) match
        case Apply(fun, args)
            if fun.symbol.name == "apply" && fun.symbol.owner == tupleModuleClass && args.length == elems.size =>
          extractArgs(args)
        case _ => None
    // A bare reference to a singleton (case object/enum case), not a call. TermRef for an explicit given/enum case, TypeRef (via companion) for `derives`.
    case t if elems.isEmpty && t.symbol == (if tpe.termSymbol.exists then tpe.termSymbol else sym.companionModule) =>
      extractArgs(Nil)
    case _ => None

  args.map(vs => m.fromProduct(Tuple.fromArray(vs.toArray)))

private[quoted] def unapplySum[T](elems: List[FromExpr[Any]])(x: Expr[T])(using Quotes): Option[T] =
  elems.iterator.map(_.unapply(x)).collectFirst { case Some(v) => v.asInstanceOf[T] }
private[quoted] def applyProduct[T: Type](elems: List[ToExpr[Any]])(x: T)(using Quotes): Expr[T] =
  val mirrorExpr = Expr.summon[Mirror.ProductOf[T]].get
  val elemVals = x.asInstanceOf[Product].productIterator.toList
  val elemExprs = elems.zip(elemVals).map(_.apply(_))
  val tupleExpr = Expr.ofTupleFromSeq(elemExprs)
  '{ $mirrorExpr.fromProduct($tupleExpr) }

private[quoted] inline def summonAllOrDerive[Elems <: Tuple, F[_]](inline derived: [A:Mirror.Of] => () => F[A]): List[F[Any]] = inline compiletime.erasedValue[Elems] match
    case _: EmptyTuple => Nil
    case _: (h *: t) => summonOrDerive[h, F](derived).asInstanceOf[F[Any]] :: summonAllOrDerive[t, F](derived)

private[quoted] inline def summonOrDerive[T, F[_]](inline derived: [A:Mirror.Of] => () => F[A]): F[T] = compiletime.summonFrom:
    case te: F[T] => te
    case given Mirror.Of[T] => derived[T]()