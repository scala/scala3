package dotty.tools
package dotc
package typer

import ast._
import core._
import Trees._
import Types._, ProtoTypes._, Contexts._, Decorators._, Denotations._, Symbols._
import Applications._, Implicits._, Flags._
import util.Positions._
import printing.{Showable, RefinedPrinter}
import scala.collection.mutable
import java.util.regex.Matcher.quoteReplacement

object ErrorReporting {

  import tpd._

  def errorTree(tree: untpd.Tree, msg: => String)(implicit ctx: Context): tpd.Tree =
    tree withType errorType(msg, tree.pos)

  def errorType(msg: => String, pos: Position)(implicit ctx: Context): ErrorType = {
    ctx.error(msg, pos)
    ErrorType
  }

  def cyclicErrorMsg(ex: CyclicReference)(implicit ctx: Context) = {
    val cycleSym = ex.denot.symbol
    def errorMsg(msg: String, cx: Context): String =
      if (cx.mode is Mode.InferringReturnType) {
        cx.tree match {
          case tree: untpd.ValOrDefDef =>
              // Dotty deviation: Was Trees.ValOrDefDef[_], but this gives ValOrDefDef[Nothing] instead of
              // ValOrDefDel[Null]. Scala handles it, but it looks accidental because bounds propagation
              // fails if the parameter is invariant or cotravariant.
              // See test pending/pos/boundspropagation.scala
            val treeSym = ctx.symOfContextTree(tree)
            if (treeSym.exists && treeSym.name == cycleSym.name && treeSym.owner == cycleSym.owner) {
              val result = if (cycleSym is Method) " result" else ""
              em"overloaded or recursive $cycleSym needs$result type"
            }
            else errorMsg(msg, cx.outer)
          case _ =>
            errorMsg(msg, cx.outer)
        }
      } else msg
    errorMsg(ex.show, ctx)
  }

  def wrongNumberOfArgs(fntpe: Type, kind: String, expected: Int, pos: Position)(implicit ctx: Context) =
    errorType(em"wrong number of ${kind}arguments for $fntpe, expected: $expected", pos)

  class Errors(implicit ctx: Context) {

    /** An explanatory note to be added to error messages
     *  when there's a problem with abstract var defs */
    def abstractVarMessage(sym: Symbol): String =
      if (sym.underlyingSymbol.is(Mutable))
        "\n(Note that variables need to be initialized to be defined)"
      else ""

    def expectedTypeStr(tp: Type): String = tp match {
      case tp: PolyProto =>
        em"type arguments [${tp.targs}%, %] and ${expectedTypeStr(tp.resultType)}"
      case tp: FunProto =>
        val result = tp.resultType match {
          case _: WildcardType | _: IgnoredProto => ""
          case tp => em" and expected result type $tp"
        }
        em"arguments (${tp.typedArgs.tpes}%, %)$result"
      case _ =>
        em"expected type $tp"
    }

    def anonymousTypeMemberStr(tpe: Type) = {
      val kind = tpe match {
          case _: TypeBounds => "type with bounds"
          case _: PolyType | _: MethodType => "method"
          case _ => "value of type"
        }
        em"$kind $tpe"
    }

    def overloadedAltsStr(alts: List[SingleDenotation]) =
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

    def patternConstrStr(tree: Tree): String = ???

    def typeMismatch(tree: Tree, pt: Type, implicitFailure: SearchFailure = NoImplicitMatches): Tree =
      errorTree(tree, typeMismatchStr(normalize(tree.tpe, pt), pt) + implicitFailure.postscript)

    /** A subtype log explaining why `found` does not conform to `expected` */
    def whyNoMatchStr(found: Type, expected: Type) =
      if (ctx.settings.explaintypes.value)
        "\n" + ctx.typerState.show + "\n" + TypeComparer.explained((found <:< expected)(_))
      else
        ""

    def typeMismatchStr(found: Type, expected: Type) = {
      // replace constrained polyparams and their typevars by their bounds where possible
      object reported extends TypeMap {
        def setVariance(v: Int) = variance = v
        val constraint = ctx.typerState.constraint
        def apply(tp: Type): Type = tp match {
          case tp: PolyParam =>
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
      ex"""|type mismatch:
           |found:    $found1
           |required: $expected1""".stripMargin + whyNoMatchStr(found, expected)
    }

    /** Format `raw` implicitNotFound argument, replacing all
     *  occurrences of `${X}` where `X` is in `paramNames` with the
     *  corresponding shown type in `args`.
     */
    def implicitNotFoundString(raw: String, paramNames: List[String], args: List[Type]): String = {
      def translate(name: String): Option[String] = {
        val idx = paramNames.indexOf(name)
        if (idx >= 0) Some(quoteReplacement(ex"${args(idx)}")) else None
      }
      """\$\{\w*\}""".r.replaceSomeIn(raw, m => translate(m.matched.drop(2).init))
    }
  }

  def err(implicit ctx: Context): Errors = new Errors
}
