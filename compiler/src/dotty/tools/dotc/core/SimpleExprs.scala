package dotty.tools.dotc
package core

import scala.collection.mutable.ListBuffer

import ast.tpd
import tpd.{Tree, Select, Apply, TypeApply, Ident, closureDef, Lambda, TreeOps, SeqLiteral, New, tpes, TypeTree, Typed, NamedArg}
import Types.{Type, TermRef, ThisType, ConstantType, TermParamRef, TypeMap, MethodType, SkolemType, NoPrefix, TypeRef, TypeParamRef}
import Contexts.{Context, ctx}
import Symbols.{Symbol, NoSymbol, defn}
import Names.Name
import StdNames.nme
import Decorators.i
import dotty.tools.dotc.util.NoSource
import dotty.tools.dotc.util.Spans.NoSpan

object SimpleExprs:
  enum SimpleExpr:
    case Leaf(tp: Type)
    case New(sym: Symbol)
    case Apply(fun: SimpleExpr, args: List[SimpleExpr])
    case TypeApply(fun: SimpleExpr, args: List[Type])
    case Select(qualifier: SimpleExpr, symbol: Symbol)
    case Typed(arg: SimpleExpr, tp: Type)
    case SeqLiteral(args: List[SimpleExpr], tp: Type)
    case NamedArg(name: Name, value: SimpleExpr)
    case TypeTree(tp: Type)
    case Lambda(mt: MethodType, params: List[Int], body: SimpleExpr)
    case ParamRef(index: Int)

  val se = SimpleExpr

  def isLeafType(tp: Type)(using Context): Boolean =
    tp.isSingleton || tp.isInstanceOf[TypeRef] || tp.isInstanceOf[TypeParamRef]

  private val paramIndex = util.EqHashMap[Symbol, Int]()
  private var maxParamIndex = 0

  def fromType(tp: Type)(using Context): Option[SimpleExpr] =
    Some(se.Leaf(if isLeafType(tp) then tp else SkolemType(tp)))

  def fromTree(t: Tree)(using Context): Option[SimpleExpr] =
    t match
      case New(tpt) =>
        // assert(tpt.symbol.exists, tpt)
        val classSym = tpt.tpe.typeSymbol
        assert(classSym.exists, tpt)
        Some(se.New(classSym))
      case Apply(fun, args) =>
        for fun1 <- fromTree(fun); args1 <- args.mapOption(fromTree) yield se.Apply(fun1, args1)
      case TypeApply(fun, args) =>
        for fun1 <- fromTree(fun) yield se.TypeApply(fun1, args.tpes)
      case Select(qual, name) =>
        for qual1 <- fromTree(qual) yield se.Select(qual1, t.symbol)
      case Typed(expr, tpt) =>
        for expr1 <- fromTree(expr) yield se.Typed(expr1, tpt.tpe)
      case SeqLiteral(args, tpt) =>
        for args1 <- args.mapOption(fromTree) yield se.SeqLiteral(args1, tpt.tpe)
      case NamedArg(name, arg) =>
        for arg1 <- fromTree(arg) yield se.NamedArg(name, arg1)
      case TypeTree() =>
        Some(se.TypeTree(t.tpe))
      case closureDef(defDef) =>
        defDef.symbol.info.dealias match
          case mt: MethodType =>
            assert(defDef.termParamss.size == 1, "closures have a single parameter list, right?")
            val params = defDef.termParamss.head
            for param <- params if !paramIndex.contains(param.symbol) do
              paramIndex(param.symbol) = maxParamIndex
              maxParamIndex += 1
            for body <- fromTree(defDef.rhs) yield se.Lambda(mt, params.map(p => paramIndex(p.symbol)), body)
          case _ => None
      case Ident(nme.???) =>
        Some(se.Leaf(SkolemType(t.tpe)))
      case _ if paramIndex.contains(t.symbol) =>
        Some(se.ParamRef(paramIndex(t.symbol)))
      case _ if isLeafType(t.tpe) =>
        fromType(t.tpe)
      case _ =>
        assert(false, s"Cannot convert ${t.show} to a SimpleExpr")
        None

  extension (e: SimpleExpr)
    def mapWith(tm: TypeMap)(using Context): Option[SimpleExpr] =
      e match
        case se.Leaf(tp) =>
          val tp1 = tm(tp)
          if tm.isRange(tp1) then None else fromType(tp1)
        case se.New(sym) =>
          Some(e)
        case se.Apply(fun, args) =>
          for fun1 <- fun.mapWith(tm); args1 <- args.mapOption(_.mapWith(tm)) yield se.Apply(fun1, args1)
        case se.TypeApply(fun, args) =>
          for fun1 <- fun.mapWith(tm) yield se.TypeApply(fun1, args.map(tm))
        case se.Select(qualifier, symbol) =>
          for qualifier1 <- qualifier.mapWith(tm) yield se.Select(qualifier1, symbol)
        case se.Typed(arg, tp) =>
          for arg1 <- arg.mapWith(tm) yield se.Typed(arg1, tm(tp))
        case se.SeqLiteral(args, tp) =>
          for args1 <- args.mapOption(_.mapWith(tm)) yield se.SeqLiteral(args1, tm(tp))
        case se.NamedArg(name, value) =>
          for value1 <- value.mapWith(tm) yield se.NamedArg(name, value1)
        case se.TypeTree(tp) =>
          Some(se.TypeTree(tm(tp)))
        case se.Lambda(mt, params, body) =>
          tm(mt) match
            case mt1: MethodType =>
              for body1 <- body.mapWith(tm) yield se.Lambda(mt1, params, body1)
            case _ => None
        case se.ParamRef(index) =>
          Some(e)

    def toTree(paramRefs: Map[Int, Tree] = Map.empty)(using Context): Tree =
      e match
        case se.Leaf(tp) =>
          def skolem(tp: Type) = ast.untpd.Ident(nme.???).withType(tp)
          tp.dealiasKeepAnnotsAndOpaques.stripTypeVar match
            case tp @ TermParamRef(binder, paramNum) =>
              val paramInfo = binder.paramInfos(paramNum)
              Ident(paramInfo.termSymbol.termRef).withType(tp)
            case _: (TermRef | ThisType | ThisType | ConstantType) => tpd.singleton(tp)
            case SkolemType(info)                                  => skolem(info)
            case tp                                                => skolem(tp)
        case se.New(sym) =>
          New(tpd.ref(sym))
        case se.Apply(fun, args) =>
          Apply(fun.toTree(paramRefs), args.map(_.toTree(paramRefs)))
        case se.TypeApply(fun, args) =>
          val fun1 = fun.toTree(paramRefs)
          if ctx.erasedTypes then fun1
          else fun1.appliedToTypeTrees(args.map(TypeTree(_)))
        case se.Typed(arg, tp) =>
          Typed(arg.toTree(paramRefs), TypeTree(tp))
        case se.Select(qualifier, symbol) =>
          qualifier.toTree(paramRefs).select(symbol)
        case se.SeqLiteral(args, tp) =>
          SeqLiteral(args.map(_.toTree(paramRefs)), TypeTree(tp))
        case se.NamedArg(name, value) =>
          NamedArg(name, value.toTree(paramRefs))
        case se.TypeTree(tp) =>
          TypeTree(tp)
        case se.Lambda(mt, params, body) =>
          Lambda(
            mt,
            (myParamRefs: List[Tree]) =>
              body.toTree(paramRefs ++ params.zip(myParamRefs))
          )
        case se.ParamRef(index) =>
          paramRefs(index)

    def existsType(p: Type => Boolean)(using Context): Boolean =
      e match
        case se.Leaf(tp)                  => p(tp)
        case se.New(sym)                  => p(sym.info)
        case se.Apply(fun, args)          => fun.existsType(p) || args.exists(_.existsType(p))
        case se.TypeApply(fun, args)      => fun.existsType(p) || args.exists(p)
        case se.Select(qualifier, symbol) => qualifier.existsType(p)
        case se.Typed(arg, tp)            => arg.existsType(p) || p(tp)
        case se.SeqLiteral(args, tp)      => args.exists(_.existsType(p)) || p(tp)
        case se.NamedArg(name, value)     => value.existsType(p)
        case se.TypeTree(tp)              => p(tp)
        case se.Lambda(mt, params, body)  => mt.paramInfos.exists(p) || p(mt.resType) || body.existsType(p)
        case se.ParamRef(index)           => false

    def show(using Context): String =
      e match
        case se.Leaf(tp)                  => tp.show
        case se.New(sym)                  => s"new ${sym.showName}"
        case se.Apply(fun, args)          => s"${fun.show}(${args.map(_.show).mkString(", ")})"
        case se.TypeApply(fun, args)      => s"${fun.show}[${args.map(_.show).mkString(", ")}]"
        case se.Select(qualifier, symbol) => s"${qualifier.show}.${symbol.showName}"
        case se.Typed(arg, tp)            => s"${arg.show}: ${tp.show}"
        case se.SeqLiteral(args, tp)      => s"[${args.map(_.show).mkString(", ")}]"
        case se.NamedArg(name, value)     => s"$name = ${value.show}"
        case se.TypeTree(tp)              => tp.show
        case se.Lambda(mt, params, body)  => s"(${params.map("x" + _).mkString(", ")}) => ${body.show}"
        case se.ParamRef(index)           => s"x$index"

  extension [T](s: List[T])
    private def mapOption[S](f: T => Option[S]): Option[List[S]] =
      var res = ListBuffer[S]()
      var current = s
      while current.nonEmpty do
        f(current.head) match
          case Some(x) =>
            res += x
            current = current.tail
          case None =>
            return None
      Some(res.toList)
