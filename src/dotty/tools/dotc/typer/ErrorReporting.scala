package dotty.tools
package dotc
package typer

import ast._
import core._
import Trees._
import Types._, ProtoTypes._, Contexts._, Decorators._, Denotations._, Symbols._
import Applications._, Implicits._, Flags._
import util.Positions._
import reporting.Diagnostic
import printing.Showable
import printing.Disambiguation.disambiguated
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
              d"overloaded or recursive $cycleSym needs$result type"
            }
            else errorMsg(msg, cx.outer)
          case _ =>
            errorMsg(msg, cx.outer)
        }
      } else msg
    errorMsg(ex.show, ctx)
  }

  class Errors(implicit ctx: Context) {

    /** An explanatory note to be added to error messages
     *  when there's a problem with abstract var defs */
    def abstractVarMessage(sym: Symbol): String =
      if (sym.underlyingSymbol.is(Mutable))
        "\n(Note that variables need to be initialized to be defined)"
      else ""

    def expectedTypeStr(tp: Type): String = tp match {
      case tp: PolyProto =>
        d"type arguments [${tp.targs}%, %] and ${expectedTypeStr(tp.resultType)}"
      case tp: FunProto =>
        val result = tp.resultType match {
          case _: WildcardType | _: IgnoredProto => ""
          case tp => d" and expected result type $tp"
        }
        d"arguments (${tp.typedArgs.tpes}%, %)$result"
      case _ =>
        d"expected type $tp"
    }

    def anonymousTypeMemberStr(tpe: Type) = {
      val kind = tpe match {
          case _: TypeBounds => "type with bounds"
          case _: PolyType | _: MethodType => "method"
          case _ => "value of type"
        }
        d"$kind $tpe"
    }

    def overloadedAltsStr(alts: List[SingleDenotation]) =
      d"overloaded alternatives of ${denotStr(alts.head)} with types\n" +
      d" ${alts map (_.info)}%\n %"

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

    def typeMismatch(tree: Tree, pt: Type, implicitFailure: SearchFailure = NoImplicitMatches): Tree = {
      errorTree(tree, typeMismatchStr(normalize(tree.tpe, pt), pt) + implicitFailure.postscript)
    }

    /** A subtype log explaining why `found` does not conform to `expected` */
    def whyNoMatchStr(found: Type, expected: Type) =
      if (ctx.settings.explaintypes.value)
        "\n" + ctx.typerState.show + "\n" + TypeComparer.explained((found <:< expected)(_))
      else
        ""

    def typeMismatchStr(found: Type, expected: Type) = disambiguated { implicit ctx =>
      def infoStr = found match { // DEBUG
          case tp: TypeRef => s"with info ${tp.info} / ${tp.prefix.toString} / ${tp.prefix.dealias.toString}"
          case _ => ""
        }
      // replace constrained polyparams and their typevars by their bounds where possible
      val reported = new TypeMap {
        def apply(tp: Type): Type = tp match {
          case tp: PolyParam =>
            val e = ctx.typerState.constraint.entry(tp)
            if (e.exists)
              if (variance > 0) e.bounds.hi
              else if (variance < 0) e.bounds.lo
              else tp
            else tp
          case tp: TypeVar => apply(tp.stripTypeVar)
          case _ => mapOver(tp)
        }
      }
      d"""type mismatch:
           | found   : $found
           | required: ${reported(expected)}""".stripMargin + whyNoMatchStr(found, expected)
    }

    /** Format `raw` implicitNotFound argument, replacing all
     *  occurrences of `${X}` where `X` is in `paramNames` with the
     *  corresponding shown type in `args`.
     */
    def implicitNotFoundString(raw: String, paramNames: List[String], args: List[Type]): String = {
      def translate(name: String): Option[String] = {
        val idx = paramNames.indexOf(name)
        if (idx >= 0) Some(quoteReplacement(args(idx).show)) else None
      }
      """\$\{\w*\}""".r.replaceSomeIn(raw, m => translate(m.matched.drop(2).init))
    }
  }

  def err(implicit ctx: Context): Errors = new Errors

  /** The d string interpolator works like the i string interpolator, but marks nonsensical errors
   *  using `<nonsensical>...</nonsensical>` tags.
   *  Note: Instead of these tags, it would be nicer to return a data structure containing the message string
   *  and a boolean indicating whether the message is sensical, but then we cannot use string operations
   *  like concatenation, stripMargin etc on the values returned by d"...", and in the current error
   *  message composition methods, this is crucial.
   */
  implicit class DiagnosticString(val sc: StringContext) extends AnyVal {
    def d(args: Any*)(implicit ctx: Context): String = {
      def isSensical(arg: Any): Boolean = arg match {
        case l: Seq[_] => l.forall(isSensical(_))
        case tpe: Type if tpe.isErroneous => false
        case NoType => false
        case sym: Symbol if sym.isCompleted =>
          sym.info != ErrorType && sym.info != TypeAlias(ErrorType) && sym.info != NoType
        case _ => true
      }

      val s = new StringInterpolators(sc).i(args : _*)
      if (args.forall(isSensical(_))) s
      else Diagnostic.nonSensicalStartTag + s + Diagnostic.nonSensicalEndTag
    }
  }
}
