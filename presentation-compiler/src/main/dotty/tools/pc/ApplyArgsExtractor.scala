package dotty.tools.pc

import scala.annotation.tailrec
import scala.util.Try

import dotty.tools.dotc.ast.Trees.ValDef
import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Denotations.MultiDenotation
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.Method
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames.*
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Symbols.defn
import dotty.tools.dotc.core.SymDenotations.NoDenotation
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.pc.IndexedContext
import dotty.tools.pc.utils.InteractiveEnrichments.*

object ApplyExtractor:
  def unapply(path: List[Tree])(using Context): Option[Apply] =
    path match
      case ValDef(_, _, _) :: Block(_, app: Apply) :: _
          if !app.fun.isInfix => Some(app)
      case rest =>
        def getApplyForContextFunctionParam(path: List[Tree]): Option[Apply] =
          path match
            // fun(arg@@)
            case (app: Apply) :: _ => Some(app)
            // fun(arg@@), where fun(argn: Context ?=> SomeType)
            // recursively matched for multiple context arguments, e.g. Context1 ?=> Context2 ?=> SomeType
            case (_: DefDef) :: Block(List(_), _: Closure) :: rest =>
              getApplyForContextFunctionParam(rest)
            case _ => None
        for
          app <- getApplyForContextFunctionParam(rest)
          if !app.fun.isInfix
        yield app
    end match

object ApplyArgsExtractor:
  def getArgsAndParams(
      optIndexedContext: Option[IndexedContext],
      apply: Apply,
      span: Span
  )(using Context): List[(List[Tree], List[ParamSymbol])] =
    def collectArgss(a: Apply): List[List[Tree]] =
      def stripContextFuntionArgument(argument: Tree): List[Tree] =
        argument match
          case Block(List(d: DefDef), _: Closure) =>
            d.rhs match
              case app: Apply =>
                app.args
              case b @ Block(List(_: DefDef), _: Closure) =>
                stripContextFuntionArgument(b)
              case _ => Nil
          case v => List(v)
      val args = a.args.flatMap(stripContextFuntionArgument)
      a.fun match
        case app: Apply => collectArgss(app) :+ args
        case _ => List(args)

    val method = apply.fun

    val argss = collectArgss(apply)

    def fallbackFindApply(sym: Symbol) =
      sym.info.member(nme.apply) match
        case NoDenotation => Nil
        case den => List(den.symbol)

      // fallback for when multiple overloaded methods match the supplied args
    def fallbackFindMatchingMethods() =
      def matchingMethodsSymbols(
          indexedContext: IndexedContext,
          method: Tree
      ): List[Symbol] =
        method match
          case Ident(name) => indexedContext.findSymbol(name).getOrElse(Nil)
          case Select(This(_), name) => indexedContext.findSymbol(name).getOrElse(Nil)
          case sel @ Select(from, name) =>
            val symbol = from.symbol
            val ownerSymbol =
              if symbol.is(Method) && symbol.owner.isClass then
                Some(symbol.owner)
              else Try(symbol.info.classSymbol).toOption
            ownerSymbol.map(sym => sym.info.member(name)).collect {
              case single: SingleDenotation => List(single.symbol)
              case multi: MultiDenotation => multi.allSymbols
            }.getOrElse(Nil)
          case Apply(fun, _) => matchingMethodsSymbols(indexedContext, fun)
          case _ => Nil

      val matchingMethods =
        for
          indexedContext <- optIndexedContext.toList
          potentialMatch <- matchingMethodsSymbols(indexedContext, method)
          if potentialMatch.is(Flags.Method) &&
            potentialMatch.vparamss.length >= argss.length &&
            Try(potentialMatch.isAccessibleFrom(apply.symbol.info)).toOption
              .getOrElse(false) &&
            potentialMatch.vparamss
              .zip(argss)
              .reverse
              .zipWithIndex
              .forall { case (pair, index) =>
                FuzzyArgMatcher(potentialMatch.tparams)
                  .doMatch(allArgsProvided = index != 0, span)
                  .tupled(pair)
              }
        yield potentialMatch

      matchingMethods
    end fallbackFindMatchingMethods

    val matchingMethods: List[Symbol] =
      if method.symbol.paramSymss.nonEmpty then
        val allArgsAreSupplied =
          val vparamss = method.symbol.vparamss
          vparamss.length == argss.length && vparamss
            .zip(argss)
            .lastOption
            .exists { case (baseParams, baseArgs) =>
              baseArgs.length == baseParams.length
            }
        // ```
        //  m(arg : Int)
        //  m(arg : Int, anotherArg : Int)
        //  m(a@@)
        // ```
        // complier will choose the first `m`, so we need to manually look for the other one
        if allArgsAreSupplied then
          val foundPotential = fallbackFindMatchingMethods()
          if foundPotential.contains(method.symbol) then foundPotential
          else method.symbol :: foundPotential
        else List(method.symbol)
      else if method.symbol.is(Method) || method.symbol == NoSymbol then
        fallbackFindMatchingMethods()
      else fallbackFindApply(method.symbol)
      end if

    matchingMethods.map { methodSym =>
      val vparamss = methodSym.vparamss

      // get params and args we are interested in
      // e.g.
      // in the following case, the interesting args and params are
      // - params: [apple, banana]
      // - args: [apple, b]
      // ```
      // def curry(x: Int)(apple: String, banana: String) = ???
      // curry(1)(apple = "test", b@@)
      // ```
      val (baseParams0, baseArgs) =
        vparamss.zip(argss).lastOption.getOrElse((Nil, Nil))

      val baseParams: List[ParamSymbol] =
        def defaultBaseParams = baseParams0.map(JustSymbol(_))

        @tailrec
        def getRefinedParams(refinedType: Type, level: Int): List[ParamSymbol] =
          if level > 0 then
            val resultTypeOpt =
              refinedType match
                case RefinedType(AppliedType(_, args), _, _) => args.lastOption
                case AppliedType(_, args) => args.lastOption
                case _ => None
            resultTypeOpt match
              case Some(resultType) => getRefinedParams(resultType, level - 1)
              case _ => defaultBaseParams
          else
            refinedType match
              case RefinedType(AppliedType(_, args), _, MethodType(ri)) =>
                baseParams0.zip(ri).zip(args).map { case ((sym, name), arg) =>
                  RefinedSymbol(sym, name, arg)
                }
              case _ => defaultBaseParams

        // finds param refinements for lambda expressions
        // val hello: (x: Int, y: Int) => Unit = (x, _) => println(x)
        @tailrec
        def refineParams(method: Tree, level: Int): List[ParamSymbol] =
          method match
            case Select(Apply(f, _), _) => refineParams(f, level + 1)
            case Select(h, name) =>
              // for Select(foo, name = apply) we want `foo.symbol`
              if name == nme.apply then getRefinedParams(h.symbol.info, level)
              else getRefinedParams(method.symbol.info, level)
            case Apply(f, _) =>
              refineParams(f, level + 1)
            case _ => getRefinedParams(method.symbol.info, level)
        refineParams(method, 0)

      end baseParams

      (baseArgs, baseParams)
    }

  extension (method: Symbol)
    def vparamss(using Context) = method.filteredParamss(_.isTerm)
    def tparams(using Context) = method.filteredParamss(_.isType).flatten
    def filteredParamss(f: Symbol => Boolean)(using Context) =
      method.paramSymss.filter(params => params.forall(f))
sealed trait ParamSymbol:
  def name: Name
  def info: Type
  def symbol: Symbol
  def nameBackticked(using Context) = name.decoded.backticked

case class JustSymbol(symbol: Symbol)(using Context) extends ParamSymbol:
  def name: Name = symbol.name
  def info: Type = symbol.info

case class RefinedSymbol(symbol: Symbol, name: Name, info: Type)
    extends ParamSymbol

class FuzzyArgMatcher(tparams: List[Symbol])(using Context):

  /** A heuristic for checking if the passed arguments match the method's
   *  arguments' types. For non-polymorphic methods we use the subtype relation
   *  (`<:<`) and for polymorphic methods we use a heuristic. We check the args
   *  types not the result type.
   */
  def doMatch(
      allArgsProvided: Boolean,
      span: Span
  )(expectedArgs: List[Symbol], actualArgs: List[Tree]) =
    (expectedArgs.length == actualArgs.length ||
      (!allArgsProvided && expectedArgs.length >= actualArgs.length)) &&
      actualArgs.zipWithIndex.forall {
        case (arg: Ident, _) if arg.span.contains(span) => true
        case (NamedArg(name, arg), _) =>
          expectedArgs.exists { expected =>
            expected.name == name && (!arg.hasType || arg.typeOpt.unfold
              .fuzzyArg_<:<(expected.info))
          }
        case (arg, i) =>
          !arg.hasType || arg.typeOpt.unfold.fuzzyArg_<:<(expectedArgs(i).info)
      }

  extension (arg: Type)
    def fuzzyArg_<:<(expected: Type) =
      if tparams.isEmpty then arg <:< expected
      else arg <:< substituteTypeParams(expected)
    def unfold =
      arg match
        case arg: TermRef => arg.underlying
        case e => e

  private def substituteTypeParams(t: Type): Type =
    t match
      case e if tparams.exists(_ == e.typeSymbol) =>
        val matchingParam = tparams.find(_ == e.typeSymbol).get
        matchingParam.info match
          case b @ TypeBounds(_, _) => WildcardType(b)
          case _ => WildcardType
      case o @ OrType(e1, e2) =>
        OrType(substituteTypeParams(e1), substituteTypeParams(e2), o.isSoft)
      case AndType(e1, e2) =>
        AndType(substituteTypeParams(e1), substituteTypeParams(e2))
      case AppliedType(et, eparams) =>
        AppliedType(et, eparams.map(substituteTypeParams))
      case _ => t

end FuzzyArgMatcher
