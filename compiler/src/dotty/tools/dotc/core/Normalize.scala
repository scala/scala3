package dotty.tools
package dotc
package core

import Contexts._
import Decorators._
import Denotations._
import Flags._
import Names._
import Symbols._
import Types._
import reporting.trace
import reporting.diagnostic.Message
import typer.ErrorReporting.errorType
import ast.tpd._
import scala.annotation.tailrec
import scala.collection.mutable

object Normalize {
  @sharable var track = true
}

private final class NormalizeMap(implicit ctx: Context) extends TypeMap {
  private[this] var fuel: Int = ctx.settings.XmaxTypeEvaluationSteps.value
  private[this] var canReduce: Boolean = true

  /** Infrastructure for beta-reduction at the type-level, to be cached per transparent method. */
  class Unfolder(fnSym: Symbol, body: Tree) {
    private[this] val paramPos = mutable.ArrayBuffer[Name]()
    private[this] var params: Array[Symbol] = _

    private def computeParamPositions(tp: Type): Unit = tp match {
      case tp: MethodOrPoly =>
        paramPos ++= tp.paramNames
        computeParamPositions(tp.resultType)
      case _ =>
    }

    private def computeOrderedParams = {
      def registerType(tp: Type): Unit = tp match {
        case tp: NamedType if tp.symbol.is(Param) && tp.symbol.owner == fnSym =>
          params(paramPos.indexOf(tp.name)) = tp.symbol
        case _ =>
      }

      params = Array.fill[Symbol](paramPos.length)(NoSymbol)
      body.tpe.foreachPart(registerType, stopAtStatic = true)
    }

    def isParameterless: Boolean = paramPos.isEmpty

    /** Performs beta-reduction for a given list of arguments, as seen from the given prefix. */
    def unfold(pre: Type, args: List[Type]): Type = {
      @tailrec def substPairs(paramPos: Int, args: List[Type],
                              from: List[Symbol], to: List[Type]): (List[Symbol], List[Type]) =
        if (paramPos == params.length)
          (from, to)
        else
          if (params(paramPos).exists) substPairs(paramPos + 1, args.tail, params(paramPos) :: from, args.head :: to)
          else                         substPairs(paramPos + 1, args.tail, from, to)

      assert(args.length == params.length)
      val (from, to) = substPairs(0, args, Nil, Nil)
      body.tpe.subst(from, to).asSeenFrom(pre, fnSym.enclosingClass)  // TODO: Check whether enclosingClass makes sense
    }

    // TODO: Cache this per transparent method
    computeParamPositions(fnSym.info)
    computeOrderedParams
  }

  private def assertOneArg(argss: List[List[Type]]): Unit =
    assert(argss.length == 1 && argss.head.length == 1, i"Expected one argument, got: $argss")

  private def asType(b: Boolean) = ConstantType(Constants.Constant(b))

  private def typeTest(actualTp: Type, testedTp: Type): Option[Boolean] =
    if (ctx.typeComparer.isSubTypeWhenFrozen(actualTp, testedTp))      Some(true)
    else if (ctx.typeComparer.isSubTypeWhenFrozen(testedTp, actualTp)) None
    else                                                               Some(false)

  /** The body type of transparent method `sym` if the body itself is not currently being type-checked,
    * error otherwise.
    */
  private def defUnfolder(fnSym: Symbol): Unfolder = {
    assert(fnSym.isTerm && fnSym.isTransparentMethod && fnSym.hasAnnotation(defn.BodyAnnot))
    val body: Tree = fnSym.getAnnotation(defn.BodyAnnot) match {
      case Some(annot) =>
        if (annot.isEvaluating)
          throw CyclicReference(fnSym)  // TODO: Better error message?
        annot.tree
      case None =>
        throw new AssertionError(s"Expected transparent method $fnSym to have a body annotation!")
    }
    new Unfolder(fnSym, body)
  }

  private def normalizeApp(fn: TermRef, argss: List[List[Type]], realApplication: Boolean): Type = {
    import dotc.typer.ConstFold

    val fnSym = fn.symbol
    // TODO: Replace `Stable` requirement by some other special case
    if (fnSym.is(Method)) {
      if (defn.ScalaValueClasses().contains(fnSym.owner)) {
        argss match {
          // TODO: Retrofit Type-entrypoint into ConstFold
          /*case List()          => ConstFold(fn)
          case List(List(arg)) => ConstFold(fn, arg)*/
          case _               => NoType
        }
      }
      else if (fnSym is Transparent) {
        // Reduction step
        // TODO(gsps): Also reduce if fnSym's finalResultType is singleton (or do this in TypeAssigner?)
        val unfolder = defUnfolder(fnSym)
        if (realApplication || unfolder.isParameterless)
          apply(unfolder.unfold(fn.prefix, argss.flatten))
        else
          NoType
      }
      else if (realApplication && ((fnSym eq defn.Any_isInstanceOf) || (fnSym eq defn.Any_asInstanceOf))) {
        import TypeErasure.erasure
        assertOneArg(argss)
        val isSubTypeOpt = typeTest(erasure(fn.prefix), erasure(argss.head.head))
        if (fnSym eq defn.Any_isInstanceOf)
          isSubTypeOpt map asType getOrElse NoType
        else
          isSubTypeOpt match {
            case Some(true) => apply(fn.prefix)
            case _          => NoType
          }
      }
      else NoType
    }
    else NoType
  }

  def normalizeTermParamSel(tp: TermRef): Type = {
    /*def argForParam(param: Symbol, vparams0: List[Symbol], argss0: List[List[Type]]): Type = {
      var vparams = vparams0
      var argss = argss0
      var args = argss.head
      argss = argss.tail
      while (vparams.nonEmpty && args.nonEmpty) {
        if (vparams.head.eq(param))
          return args.head
        vparams = vparams.tail
        args = args.tail
        if (args.isEmpty && argss.nonEmpty) {
          args = argss.head
          argss = argss.tail
        }
      }
      NoType
    }

    val param = tp.symbol
    val cls = param.owner
    if (cls.flagsUNSAFE.is(Transparent)) {
      val termParams = cls.termParams
      if (termParams.exists(_.name eq param.name))
        tp.prefix.baseType(cls) match {
          case base: AppliedTermRef => argForParam(param, termParams, base.underlyingFnAndArgss._2)
          case base => NoType
        }
      else NoType
    }
    else NoType*/
    // TODO: Add termParams infra to ClassDenotations
    NoType
  }

  private def bigStep(tp: Type): Type = tp match {
    case tp @ TypeOf.If(cond, thenb, elseb) =>
      apply(cond) match {
        case ConstantType(c) if c.tag == Constants.BooleanTag =>
          if (c.value.asInstanceOf[Boolean]) apply(thenb)
          else                               apply(elseb)
        case cond1 =>
          canReduce = false
          TypeOf.If.derived(tp)(cond1, thenb, elseb)
      }

    case tp @ TypeOf.Match(selector, cases) =>
      tp  // TODO

    case tp =>
      mapOver(tp) match {
        case _ if !canReduce =>
          tp

        case tp: TermRef =>
          normalizeApp(tp, Nil, realApplication = false) orElse normalizeTermParamSel(tp) orElse {
            tp.underlying match {
              case underTp: SingletonType => apply(underTp)
              case underTp => tp
            }
          }

        case tp @ TypeOf.Call(fn, argss) =>
          normalizeApp(fn, argss, realApplication = true) orElse tp

        case tp =>
//          val tp1 = tp.stripTypeVar.dealias.widenExpr
//          if (tp eq tp1) tp else apply(tp1)
          tp
      }
  }

  def apply(tp: Type): Type = trace.conditionally(Normalize.track, i"normalize($tp)", show = true) {
    if (fuel == 0)
      errorType(i"Diverged while normalizing $tp (${ctx.settings.XmaxTypeEvaluationSteps.value} steps)", ctx.tree.pos)
    else {
      fuel -= 1
      bigStep(tp)
    }
  }
}
