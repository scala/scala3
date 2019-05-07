package dotty.tools
package dotc
package typer

import ast._
import core._
import Types._, ProtoTypes._, Contexts._, Decorators._, Denotations._, Symbols._
import Implicits._, Flags._
import util.Spans._
import util.SourcePosition
import java.util.regex.Matcher.quoteReplacement
import reporting.diagnostic.Message
import reporting.diagnostic.messages._

object ErrorReporting {

  import tpd._

  def errorTree(tree: untpd.Tree, msg: => Message, pos: SourcePosition, sticky: Boolean = false)(implicit ctx: Context): tpd.Tree =
    tree.withType(errorType(msg, pos, sticky))

  def errorTree(tree: untpd.Tree, msg: => Message)(implicit ctx: Context): tpd.Tree =
    errorTree(tree, msg, tree.sourcePos)

  def errorType(msg: => Message, pos: SourcePosition, sticky: Boolean = false)(implicit ctx: Context): ErrorType = {
    ctx.error(msg, pos, sticky)
    ErrorType(msg)
  }

  def wrongNumberOfTypeArgs(fntpe: Type, expectedArgs: List[ParamInfo], actual: List[untpd.Tree], pos: SourcePosition)(implicit ctx: Context): ErrorType =
    errorType(WrongNumberOfTypeArgs(fntpe, expectedArgs, actual)(ctx), pos)

  class Errors(implicit ctx: Context) {

    /** An explanatory note to be added to error messages
     *  when there's a problem with abstract var defs */
    def abstractVarMessage(sym: Symbol): String =
      if (sym.underlyingSymbol.is(Mutable))
        "\n(Note that variables need to be initialized to be defined)"
      else ""

    def expectedTypeStr(tp: Type): String = tp match {
      case tp: PolyProto =>
        em"type arguments [${tp.targs.tpes}%, %] and ${expectedTypeStr(tp.resultType)}"
      case tp: FunProto =>
        val result = tp.resultType match {
          case _: WildcardType | _: IgnoredProto => ""
          case tp => em" and expected result type $tp"
        }
        em"arguments (${tp.unforcedTypedArgs.tpes}%, %)$result"
      case _ =>
        em"expected type $tp"
    }

    def anonymousTypeMemberStr(tpe: Type): String = {
      val kind = tpe match {
          case _: TypeBounds => "type with bounds"
          case _: MethodOrPoly => "method"
          case _ => "value of type"
        }
        em"$kind $tpe"
    }

    def overloadedAltsStr(alts: List[SingleDenotation]): String =
      em"overloaded alternatives of ${denotStr(alts.head)} with types\n" +
      em" ${alts map (_.info)}%\n %"

    def denotStr(denot: Denotation): String =
      if (denot.isOverloaded) overloadedAltsStr(denot.alternatives)
      else if (denot.symbol.exists) denot.symbol.showLocated
      else anonymousTypeMemberStr(denot.info)

    def refStr(tp: Type): String = tp match {
      case tp: NamedType => denotStr(tp.denot)
      case _ => anonymousTypeMemberStr(tp)
    }

    def exprStr(tree: Tree): String = refStr(tree.tpe)

    def takesNoParamsStr(tree: Tree, kind: String): String =
      if (tree.tpe.widen.exists)
        i"${exprStr(tree)} does not take ${kind}parameters"
      else {
        if (ctx.settings.Ydebug.value) new FatalError("").printStackTrace()
        i"undefined: $tree # ${tree.uniqueId}: ${tree.tpe.toString} at ${ctx.phase}"
      }

    def patternConstrStr(tree: Tree): String = ???

    def typeMismatch(tree: Tree, pt: Type, implicitFailure: SearchFailureType = NoMatchingImplicits): Tree = {
      val normTp = normalize(tree.tpe, pt)
      val treeTp = if (normTp <:< pt) tree.tpe else normTp
        // use normalized type if that also shows an error, original type otherwise
      errorTree(tree, typeMismatchMsg(treeTp, pt, implicitFailure.whyNoConversion))
    }

    /** A subtype log explaining why `found` does not conform to `expected` */
    def whyNoMatchStr(found: Type, expected: Type): String = {
      def dropJavaMethod(tp: Type): Type = tp match {
        case tp: PolyType =>
          tp.derivedLambdaType(resType = dropJavaMethod(tp.resultType))
        case tp: MethodType if tp.isJavaMethod =>
          MethodType(tp.paramNames, tp.paramInfos, dropJavaMethod(tp.resultType))
        case tp => tp
      }
      val found1 = dropJavaMethod(found)
      val expected1 = dropJavaMethod(expected)
      if ((found1 eq found) != (expected eq expected1) && (found1 <:< expected1))
        i"""
           |(Note that Scala's and Java's representation of this type differs)"""
      else if (ctx.settings.explainTypes.value)
        i"""
           |${ctx.typerState.constraint}
           |${TypeComparer.explained((found <:< expected)(_))}"""
      else
        ""
    }

    def typeMismatchMsg(found: Type, expected: Type, postScript: String = ""): TypeMismatch = {
      // replace constrained TypeParamRefs and their typevars by their bounds where possible
      // the idea is that if the bounds are also not-subtypes of each other to report
      // the type mismatch on the bounds instead of the original TypeParamRefs, since
      // these are usually easier to analyze.
      object reported extends TypeMap {
        def setVariance(v: Int) = variance = v
        val constraint = ctx.typerState.constraint
        def apply(tp: Type): Type = tp match {
          case tp: TypeParamRef =>
            constraint.entry(tp) match {
              case bounds: TypeBounds =>
                if (variance < 0) apply(constraint.fullUpperBound(tp))
                else if (variance > 0) apply(constraint.fullLowerBound(tp))
                else tp
              case NoType => tp
              case instType => apply(instType)
            }
          case tp: TypeVar => apply(tp.stripTypeVar)
          case _ => mapOver(tp)
        }
      }
      val found1 = reported(found)
      reported.setVariance(-1)
      val expected1 = reported(expected)
      val (found2, expected2) =
        if (found1 frozen_<:< expected1) (found, expected) else (found1, expected1)
      TypeMismatch(found2, expected2, whyNoMatchStr(found, expected), postScript)
    }

    /** Format `raw` implicitNotFound or implicitAmbiguous argument, replacing
     *  all occurrences of `${X}` where `X` is in `paramNames` with the
     *  corresponding shown type in `args`.
     */
    def userDefinedErrorString(raw: String, paramNames: List[String], args: List[Type]): String = {
      def translate(name: String): Option[String] = {
        val idx = paramNames.indexOf(name)
        if (idx >= 0) Some(quoteReplacement(ex"${args(idx)}")) else None
      }
      """\$\{\w*\}""".r.replaceSomeIn(raw, m => translate(m.matched.drop(2).init))
    }

    def rewriteNotice: String =
      if (ctx.scala2Mode) "\nThis patch can be inserted automatically under -rewrite."
      else ""
  }

  def err(implicit ctx: Context): Errors = new Errors
}
