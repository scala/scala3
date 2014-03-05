package dotty.tools
package dotc
package typer

import ast._
import core._
import Trees._
import Types._, ProtoTypes._, Contexts._, Decorators._, Denotations._, Symbols._
import Applications._, Implicits._
import util.Positions._
import printing.Showable
import printing.Disambiguation.disambiguated
import reporting.Reporter.SuppressedMessage

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
          case tree: Trees.ValOrDefDef[_] =>
            val treeSym = ctx.symOfContextTree(tree)
            if (treeSym.exists && treeSym.name == cycleSym.name && treeSym.owner == cycleSym.owner) {
              val result = if (cycleSym.isSourceMethod) " result" else ""
              i"overloaded or recursive $cycleSym needs$result type"
            }
            else errorMsg(msg, cx.outer)
          case _ =>
            errorMsg(msg, cx.outer)
        }
      } else msg
    errorMsg(ex.show, ctx)
  }

  class Errors(implicit ctx: Context) {

    def expectedTypeStr(tp: Type): String = tp match {
      case tp: FunProto =>
        val result = tp.resultType match {
          case tp: WildcardType => ""
          case tp => i"and expected result type $tp"
        }
        i"arguments (${tp.typedArgs.tpes}%, %)$result"
      case _ =>
        i"expected type $tp"
    }

    def anonymousTypeMemberStr(tpe: Type) = {
      val kind = tpe match {
          case _: TypeBounds => "type with bounds"
          case _: PolyType | _: MethodType => "method"
          case _ => "value of type"
        }
        i"$kind $tpe"
    }

    def overloadedAltsStr(alts: List[SingleDenotation]) =
      i"overloaded alternatives of ${denotStr(alts.head)} with types\n" +
      i" ${alts map (_.info)}%\n %"

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
      errorTree(tree, typeMismatchStr(tree.tpe, pt) + implicitFailure.postscript)
    }

    def typeMismatchStr(found: Type, expected: Type) = disambiguated { implicit ctx =>
      val (typerStateStr, explanationStr) =
        if (ctx.settings.explaintypes.value)
          ("\n" + ctx.typerState.show, "\n" + TypeComparer.explained((found <:< expected)(_)))
        else
          ("", "")
      def infoStr = found match { // DEBUG
          case tp: TypeRef => s"with info ${tp.info} / ${tp.prefix.toString} / ${tp.prefix.dealias.toString}"
          case _ => ""
        }
      i"""type mismatch:
           | found   : $found
           | required: $expected""".stripMargin + typerStateStr + explanationStr
    }
  }

  def err(implicit ctx: Context): Errors = new Errors

  /** Implementation of i"..." string interpolator */
  implicit class InfoString(val sc: StringContext) extends AnyVal {

    def i(args: Any*)(implicit ctx: Context): String = {

      def isSensical(arg: Any): Boolean = arg match {
        case tpe: Type if tpe.isErroneous => false
        case NoType => false
        case sym: Symbol if sym.isCompleted =>
          sym.info != ErrorType && sym.info != TypeAlias(ErrorType) && sym.info != NoType
        case _ => true
      }

      def treatArg(arg: Any, suffix: String): (Any, String) = arg match {
        case arg: List[_] if suffix.nonEmpty && suffix.head == '%' =>
          val (rawsep, rest) = suffix.tail.span(_ != '%')
          val sep = StringContext.treatEscapes(rawsep)
          if (rest.nonEmpty) (arg map treatSingleArg mkString sep, rest.tail)
          else (arg, suffix)
        case _ =>
          (treatSingleArg(arg), suffix)
      }

      def treatSingleArg(arg: Any) : Any = arg match {
        case arg: Showable => arg.show
        case _ => arg
      }

      if (ctx.reporter.hasErrors &&
          ctx.suppressNonSensicalErrors &&
          !ctx.settings.YshowSuppressedErrors.value &&
          !args.forall(isSensical(_)))
        throw new SuppressedMessage
      val prefix :: suffixes = sc.parts.toList
      val (args1, suffixes1) = (args, suffixes).zipped.map(treatArg(_, _)).unzip
      new StringContext(prefix :: suffixes1.toList: _*).s(args1: _*)
    }
  }
}