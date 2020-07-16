package dotty.tools
package dotc
package typer

import ast._
import core._
import Types._, ProtoTypes._, Contexts._, Decorators._, Denotations._, Symbols._
import Implicits._, Flags._, Constants.Constant
import util.Spans._
import util.SourcePosition
import config.Feature
import java.util.regex.Matcher.quoteReplacement
import reporting._

object ErrorReporting {

  import tpd._

  def errorTree(tree: untpd.Tree, msg: Message, pos: SourcePosition)(using Context): tpd.Tree =
    tree.withType(errorType(msg, pos))

  def errorTree(tree: untpd.Tree, msg: Message)(using Context): tpd.Tree =
    errorTree(tree, msg, tree.sourcePos)

  def errorTree(tree: untpd.Tree, msg: TypeError, pos: SourcePosition)(using Context): tpd.Tree =
    tree.withType(errorType(msg, pos))

  def errorType(msg: Message, pos: SourcePosition)(using Context): ErrorType = {
    report.error(msg, pos)
    ErrorType(msg)
  }

  def errorType(ex: TypeError, pos: SourcePosition)(using Context): ErrorType = {
    report.error(ex, pos)
    ErrorType(ex.toMessage)
  }

  def wrongNumberOfTypeArgs(fntpe: Type, expectedArgs: List[ParamInfo], actual: List[untpd.Tree], pos: SourcePosition)(using Context): ErrorType =
    errorType(WrongNumberOfTypeArgs(fntpe, expectedArgs, actual), pos)

  class Errors(using Context) {

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
        em"arguments (${tp.typedArgs().tpes}%, %)$result"
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
        i"undefined: $tree # ${tree.uniqueId}: ${tree.tpe.toString} at ${currentPhase}"
      }

    def patternConstrStr(tree: Tree): String = ???

    def typeMismatch(tree: Tree, pt: Type, implicitFailure: SearchFailureType = NoMatchingImplicits): Tree = {
      val normTp = normalize(tree.tpe, pt)
      val treeTp = if (normTp <:< pt) tree.tpe else normTp
        // use normalized type if that also shows an error, original type otherwise
      def missingElse = tree match
        case If(_, _, elsep @ Literal(Constant(()))) if elsep.span.isSynthetic =>
          "\nMaybe you are missing an else part for the conditional?"
        case _ => ""
      errorTree(tree, TypeMismatch(treeTp, pt, implicitFailure.whyNoConversion, missingElse))
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
           |${TypeComparer.explained(found <:< expected)}"""
      else
        ""
    }

    /** Format `raw` implicitNotFound or implicitAmbiguous argument, replacing
     *  all occurrences of `${X}` where `X` is in `paramNames` with the
     *  corresponding shown type in `args`.
     */
    def userDefinedErrorString(raw: String, paramNames: List[String], args: List[Type]): String = {
      def translate(name: String): Option[String] = {
        assert(paramNames.length == args.length)
        val idx = paramNames.indexOf(name)
        if (idx >= 0) Some(quoteReplacement(ex"${args(idx)}")) else None
      }
      """\$\{\w*\}""".r.replaceSomeIn(raw, m => translate(m.matched.drop(2).init))
    }

    def rewriteNotice: String =
      if Feature.migrateTo3 then "\nThis patch can be inserted automatically under -rewrite."
      else ""

    def selectErrorAddendum
      (tree: untpd.RefTree, qual1: Tree, qualType: Type, suggestImports: Type => String)
      (using Context): String =
      val attempts: List[Tree] = qual1.getAttachment(Typer.HiddenSearchFailure) match
        case Some(failures) =>
          for failure <- failures
              if !failure.reason.isInstanceOf[Implicits.NoMatchingImplicits]
          yield failure.tree
        case _ => Nil
      if qualType.derivesFrom(defn.DynamicClass) then
        "\npossible cause: maybe a wrong Dynamic method signature?"
      else if attempts.nonEmpty then
        val attemptStrings = attempts.map(_.showIndented(4)).distinct
        val extMethods =
          if attemptStrings.length > 1 then "Extension methods were"
          else "An extension method was"
        i""".
           |$extMethods tried, but could not be fully constructed:
           |
           |    $attemptStrings%\nor\n    %"""
      else if tree.hasAttachment(desugar.MultiLineInfix) then
        i""".
           |Note that `${tree.name}` is treated as an infix operator in Scala 3.
           |If you do not want that, insert a `;` or empty line in front
           |or drop any spaces behind the operator."""
      else if qualType.isBottomType then
        ""
      else
        val add = suggestImports(
          ViewProto(qualType.widen,
            SelectionProto(tree.name, WildcardType, NoViewsAllowed, privateOK = false)))
        if add.isEmpty then ""
        else ", but could be made available as an extension method." ++ add
    end selectErrorAddendum
  }

  def dependentStr =
    """Term-dependent types are experimental,
      |they must be enabled with a `experimental.dependent` language import or setting""".stripMargin

  def err(using Context): Errors = new Errors
}
