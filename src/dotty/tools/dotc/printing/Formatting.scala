package dotty.tools.dotc
package printing

import core._
import Texts._, Types._, Flags._, Names._, Symbols._, NameOps._, Contexts._
import collection.mutable
import collection.Map
import Decorators._
import scala.annotation.switch
import scala.util.control.NonFatal
import reporting.Diagnostic

object Formatting {

  /** General purpose string formatter, with the following features:
   *
   *  1) On all Showables, `show` is called instead of `toString`
   *  2) Exceptions raised by a `show` are handled by falling back to `toString`.
   *  3) Sequences can be formatted using the desired separator between two `%` signs,
   *     eg `i"myList = (${myList}%, %)"`
   *  4) Safe handling of multi-line margins. Left margins are skipped om the parts
   *     of the string context *before* inserting the arguments. That way, we guard
   *     against accidentally treating an interpolated value as a margin.
   */
  class StringFormatter(protected val sc: StringContext) {

    protected def showArg(arg: Any)(implicit ctx: Context): String = arg match {
      case arg: Showable =>
        try arg.show(ctx.addMode(Mode.FutureDefsOK))
        catch {
          case NonFatal(ex) => s"(missing due to $ex)"
        }
      case _ => arg.toString
    }

    private def treatArg(arg: Any, suffix: String)(implicit ctx: Context): (Any, String) = arg match {
      case arg: Seq[_] if suffix.nonEmpty && suffix.head == '%' =>
        val (rawsep, rest) = suffix.tail.span(_ != '%')
        val sep = StringContext.treatEscapes(rawsep)
        if (rest.nonEmpty) (arg.map(showArg).mkString(sep), rest.tail)
        else (arg, suffix)
      case _ =>
        (showArg(arg), suffix)
    }

    def assemble(args: Seq[Any])(implicit ctx: Context): String = {
      def isLineBreak(c: Char) = c == '\n' || c == '\f' // compatible with StringLike#isLineBreak
      def stripTrailingPart(s: String) = {
        val (pre, post) = s.span(c => !isLineBreak(c))
        pre ++ post.stripMargin
      }
      val (prefix, suffixes) = sc.parts.toList match {
        case head :: tail => (head.stripMargin, tail map stripTrailingPart)
        case Nil => ("", Nil)
      }
      val (args1, suffixes1) = (args, suffixes).zipped.map(treatArg(_, _)).unzip
      new StringContext(prefix :: suffixes1.toList: _*).s(args1: _*)
    }
  }

  /** The d string interpolator works like the i string interpolator, but marks nonsensical errors
   *  using `<nonsensical>...</nonsensical>` tags.
   *  Note: Instead of these tags, it would be nicer to return a data structure containing the message string
   *  and a boolean indicating whether the message is sensical, but then we cannot use string operations
   *  like concatenation, stripMargin etc on the values returned by d"...", and in the current error
   *  message composition methods, this is crucial.
   */
  class ErrorMessageFormatter(sc: StringContext) extends StringFormatter(sc) {
    override protected def showArg(arg: Any)(implicit ctx: Context): String = {
      def isSensical(arg: Any): Boolean = arg match {
        case tpe: Type =>
          tpe.exists && !tpe.isErroneous
        case sym: Symbol if sym.isCompleted =>
          sym.info != ErrorType && sym.info != TypeAlias(ErrorType) && sym.info.exists
        case _ => true
      }
      val str = super.showArg(arg)
      if (isSensical(arg)) str else Diagnostic.nonSensicalStartTag + str + Diagnostic.nonSensicalEndTag
    }
  }

  private type Recorded = AnyRef /*Symbol | PolyParam*/

  private class Seen extends mutable.HashMap[String, List[Recorded]] {

    override def default(key: String) = Nil

    def record(str: String, entry: Recorded): String = {
      var alts = apply(str).dropWhile(entry ne _)
      if (alts.isEmpty) {
        alts = entry :: apply(str)
        update(str, alts)
      }
      str + "'" * (alts.length - 1)
    }
  }

  private class ExplainingPrinter(seen: Seen)(_ctx: Context) extends RefinedPrinter(_ctx) {
    override def simpleNameString(sym: Symbol): String =
      if ((sym is ModuleClass) && sym.sourceModule.exists) simpleNameString(sym.sourceModule)
      else seen.record(super.simpleNameString(sym), sym)

    override def polyParamNameString(param: PolyParam): String =
      seen.record(super.polyParamNameString(param), param)
  }

  def explained2(op: Context => String)(implicit ctx: Context): String = {
    val seen = new Seen
    val explainCtx = ctx.printer match {
      case dp: ExplainingPrinter => ctx // re-use outer printer and defer explanation to it
      case _ => ctx.fresh.setPrinterFn(ctx => new ExplainingPrinter(seen)(ctx))
    }

    def explanation(entry: Recorded): String = {
      def boundStr(bound: Type, default: ClassSymbol, cmp: String) =
        if (bound.isRef(default)) "" else i"$cmp $bound"

      def boundsStr(bounds: TypeBounds): String = {
        val lo = boundStr(bounds.lo, defn.NothingClass, ">:")
        val hi = boundStr(bounds.hi, defn.AnyClass, "<:")
        if (lo.isEmpty) hi
        else if (hi.isEmpty) lo
        else s"$lo and $hi"
      }

      def addendum(cat: String, info: Type)(implicit ctx: Context): String = info match {
        case bounds @ TypeBounds(lo, hi) if bounds ne TypeBounds.empty =>
          if (lo eq hi) i" which is an alias of $lo"
          else i" with $cat ${boundsStr(bounds)}"
        case _ =>
          ""
      }

      entry match {
        case param: PolyParam =>
          s"is a type variable${addendum("constraint", ctx.typeComparer.bounds(param))}"
        case sym: Symbol =>
          val ownerStr =
            if (!sym.exists) ""
            else {
              var owner = sym.effectiveOwner
              if (owner.isLocalDummy) i" locally defined in ${owner.owner}"
              else i" in $owner"
            }
          s"is a ${ctx.printer.kindString(sym)}${sym.showExtendedLocation}${addendum("bounds", sym.info)}"
      }
    }

    def explanations(seen: Seen)(implicit ctx: Context): String = {
      def needsExplanation(entry: Recorded) = entry match {
        case param: PolyParam => ctx.typerState.constraint.contains(param)
        case _ => false
      }
      val toExplain: List[(String, Recorded)] = seen.toList.flatMap {
        case (str, entry :: Nil) =>
          if (needsExplanation(entry)) (str, entry) :: Nil else Nil
        case (str, entries) =>
          entries.map(alt => (seen.record(str, alt), alt))
      }.sortBy(_._1)
      val explainParts = toExplain.map { case (str, entry) => (str, explanation(entry)) }
      val explainLines = columnar(explainParts, "  ")
      if (explainLines.isEmpty) "" else i"\n\nwhere  $explainLines%\n       %\n"
    }

    op(explainCtx) ++ explanations(seen)
  }

  def columnar(parts: List[(String, String)], sep: String): List[String] = {
    lazy val maxLen = parts.map(_._1.length).max
    parts.map {
      case (leader, trailer) =>
        s"$leader${" " * (maxLen - leader.length)}$sep$trailer"
    }
  }
}
