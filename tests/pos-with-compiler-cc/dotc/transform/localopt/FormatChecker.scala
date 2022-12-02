package dotty.tools.dotc
package transform.localopt

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.chaining.*
import scala.util.matching.Regex.Match


import PartialFunction.cond

import dotty.tools.dotc.ast.tpd.{Match => _, *}
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Phases.typerPhase
import dotty.tools.dotc.util.Spans.Span

/** Formatter string checker. */
class TypedFormatChecker(partsElems: List[Tree], parts: List[String], args: List[Tree])(using Context):

  val argTypes = args.map(_.tpe)
  val actuals = ListBuffer.empty[Tree]

  // count of args, for checking indexes
  val argc = argTypes.length

  // Pick the first runtime type which the i'th arg can satisfy.
  // If conversion is required, implementation must emit it.
  def argType(argi: Int, types: Type*): Type =
    require(argi < argc, s"$argi out of range picking from $types")
    val tpe = argTypes(argi)
    types.find(t => argConformsTo(argi, tpe, t))
      .orElse(types.find(t => argConvertsTo(argi, tpe, t)))
      .getOrElse {
        report.argError(s"Found: ${tpe.show}, Required: ${types.map(_.show).mkString(", ")}", argi)
        actuals += args(argi)
        types.head
      }

  object formattableTypes:
    val FormattableType = requiredClassRef("java.util.Formattable")
    val BigIntType      = requiredClassRef("scala.math.BigInt")
    val BigDecimalType  = requiredClassRef("scala.math.BigDecimal")
    val CalendarType    = requiredClassRef("java.util.Calendar")
    val DateType        = requiredClassRef("java.util.Date")
  import formattableTypes.*
  def argConformsTo(argi: Int, arg: Type, target: Type): Boolean = (arg <:< target).tap(if _ then actuals += args(argi))
  def argConvertsTo(argi: Int, arg: Type, target: Type): Boolean =
    import typer.Implicits.SearchSuccess
    atPhase(typerPhase) {
      ctx.typer.inferView(args(argi), target) match
        case SearchSuccess(view, ref, _, _) => actuals += view ; true
        case _ => false
    }

  // match a conversion specifier
  val formatPattern = """%(?:(\d+)\$)?([-#+ 0,(<]+)?(\d+)?(\.\d+)?([tT]?[%a-zA-Z])?""".r

  // ordinal is the regex group index in the format pattern
  enum SpecGroup:
    case Spec, Index, Flags, Width, Precision, CC
  import SpecGroup.*

  /** For N part strings and N-1 args to interpolate, normalize parts and check arg types.
   *
   *  Returns normalized part strings and args, where args correcpond to conversions in tail of parts.
   */
  def checked: (List[String], List[Tree]) =
    val amended = ListBuffer.empty[String]
    val convert = ListBuffer.empty[Conversion]

    @tailrec
    def loop(remaining: List[String], n: Int): Unit =
      remaining match
        case part0 :: more =>
          def badPart(t: Throwable): String = "".tap(_ => report.partError(t.getMessage.nn, index = n, offset = 0))
          val part = try StringContext.processEscapes(part0) catch badPart
          val matches = formatPattern.findAllMatchIn(part)

          def insertStringConversion(): Unit =
            amended += "%s" + part
            convert += Conversion(formatPattern.findAllMatchIn("%s").next(), n)  // improve
            argType(n-1, defn.AnyType)
          def errorLeading(op: Conversion) = op.errorAt(Spec)(s"conversions must follow a splice; ${Conversion.literalHelp}")
          def accept(op: Conversion): Unit =
            if !op.isLeading then errorLeading(op)
            op.accepts(argType(n-1, op.acceptableVariants*))
            amended += part
            convert += op

          // after the first part, a leading specifier is required for the interpolated arg; %s is supplied if needed
          if n == 0 then amended += part
          else if !matches.hasNext then insertStringConversion()
          else
            val cv = Conversion(matches.next(), n)
            if cv.isLiteral then insertStringConversion()
            else if cv.isIndexed then
              if cv.index.getOrElse(-1) == n then accept(cv) else insertStringConversion()
            else if !cv.isError then accept(cv)

          // any remaining conversions in this part must be either literals or indexed
          while matches.hasNext do
            val cv = Conversion(matches.next(), n)
            if n == 0 && cv.hasFlag('<') then cv.badFlag('<', "No last arg")
            else if !cv.isLiteral && !cv.isIndexed then errorLeading(cv)

          loop(more, n + 1)
        case Nil => ()
    end loop

    loop(parts, n = 0)
    if reported then (Nil, Nil)
    else
      assert(argc == actuals.size, s"Expected ${argc} args but got ${actuals.size} for [${parts.mkString(", ")}]")
      (amended.toList, actuals.toList)
  end checked

  extension (descriptor: Match)
    def at(g: SpecGroup): Int = descriptor.start(g.ordinal)
    def end(g: SpecGroup): Int = descriptor.end(g.ordinal)
    def offset(g: SpecGroup, i: Int = 0): Int = at(g) + i
    def group(g: SpecGroup): Option[String] = Option(descriptor.group(g.ordinal))
    def stringOf(g: SpecGroup): String = group(g).getOrElse("")
    def intOf(g: SpecGroup): Option[Int] = group(g).map(_.toInt)

  extension (inline value: Boolean)
    inline def or(inline body: => Unit): Boolean     = value || { body ; false }
    inline def orElse(inline body: => Unit): Boolean = value || { body ; true }
    inline def and(inline body: => Unit): Boolean    = value && { body ; true }
    inline def but(inline body: => Unit): Boolean    = value && { body ; false }

  enum Kind:
    case StringXn, HashXn, BooleanXn, CharacterXn, IntegralXn, FloatingPointXn, DateTimeXn, LiteralXn, ErrorXn
  import Kind.*

  /** A conversion specifier matched in the argi'th string part, with `argc` arguments to interpolate.
   */
  final class Conversion(val descriptor: Match, val argi: Int, val kind: Kind):
    // the descriptor fields
    val index: Option[Int]     = descriptor.intOf(Index)
    val flags: String          = descriptor.stringOf(Flags)
    val width: Option[Int]     = descriptor.intOf(Width)
    val precision: Option[Int] = descriptor.group(Precision).map(_.drop(1).toInt)
    val op: String             = descriptor.stringOf(CC)

    // the conversion char is the head of the op string (but see DateTimeXn)
    val cc: Char =
      kind match
        case ErrorXn => if op.isEmpty then '?' else op(0)
        case DateTimeXn => if op.length > 1 then op(1) else '?'
        case _ => op(0)

    def isIndexed: Boolean = index.nonEmpty || hasFlag('<')
    def isError: Boolean   = kind == ErrorXn
    def isLiteral: Boolean = kind == LiteralXn

    // descriptor is at index 0 of the part string
    def isLeading: Boolean = descriptor.at(Spec) == 0

    // true if passes.
    def verify: Boolean =
      // various assertions
      def goodies = goodFlags && goodIndex
      def noFlags = flags.isEmpty or errorAt(Flags)("flags not allowed")
      def noWidth = width.isEmpty or errorAt(Width)("width not allowed")
      def noPrecision = precision.isEmpty or errorAt(Precision)("precision not allowed")
      def only_-(msg: String) =
        val badFlags = flags.filterNot { case '-' | '<' => true case _ => false }
        badFlags.isEmpty or badFlag(badFlags(0), s"Only '-' allowed for $msg")
      def goodFlags =
        val badFlags = flags.filterNot(okFlags.contains)
        for f <- badFlags do badFlag(f, s"Illegal flag '$f'")
        badFlags.isEmpty
      def goodIndex =
        if index.nonEmpty && hasFlag('<') then warningAt(Index)("Argument index ignored if '<' flag is present")
        val okRange = index.map(i => i > 0 && i <= argc).getOrElse(true)
        okRange || hasFlag('<') or errorAt(Index)("Argument index out of range")
    // begin verify
      kind match
        case StringXn        => goodies
        case BooleanXn       => goodies
        case HashXn          => goodies
        case CharacterXn     => goodies && noPrecision && only_-("c conversion")
        case IntegralXn      =>
          def d_# = cc == 'd' && hasFlag('#') and badFlag('#', "# not allowed for d conversion")
          def x_comma = cc != 'd' && hasFlag(',') and badFlag(',', "',' only allowed for d conversion of integral types")
          goodies && noPrecision && !d_# && !x_comma
        case FloatingPointXn =>
          goodies && (cc match
            case 'a' | 'A' =>
              val badFlags = ",(".filter(hasFlag)
              noPrecision && badFlags.isEmpty or badFlags.foreach(badf => badFlag(badf, s"'$badf' not allowed for a, A"))
            case _ => true
          )
        case DateTimeXn      =>
          def hasCC = op.length == 2 or errorAt(CC)("Date/time conversion must have two characters")
          def goodCC = "HIklMSLNpzZsQBbhAaCYyjmdeRTrDFc".contains(cc) or errorAt(CC, 1)(s"'$cc' doesn't seem to be a date or time conversion")
          goodies && hasCC && goodCC && noPrecision && only_-("date/time conversions")
        case LiteralXn       =>
          op match
            case "%" => goodies && noPrecision and width.foreach(_ => warningAt(Width)("width ignored on literal"))
            case "n" => noFlags && noWidth && noPrecision
        case ErrorXn         =>
          errorAt(CC)(s"illegal conversion character '$cc'")
          false
    end verify

    // is the specifier OK with the given arg
    def accepts(arg: Type): Boolean =
      kind match
        case BooleanXn  => arg == defn.BooleanType orElse warningAt(CC)("Boolean format is null test for non-Boolean")
        case IntegralXn =>
          arg == BigIntType || !cond(cc) {
            case 'o' | 'x' | 'X' if hasAnyFlag("+ (") => "+ (".filter(hasFlag).foreach(bad => badFlag(bad, s"only use '$bad' for BigInt conversions to o, x, X")) ; true
          }
        case _ => true

    // what arg type if any does the conversion accept
    def acceptableVariants: List[Type] =
      kind match
        case StringXn        => if hasFlag('#') then FormattableType :: Nil else defn.AnyType :: Nil
        case BooleanXn       => defn.BooleanType :: defn.NullType :: Nil
        case HashXn          => defn.AnyType :: Nil
        case CharacterXn     => defn.CharType :: defn.ByteType :: defn.ShortType :: defn.IntType :: Nil
        case IntegralXn      => defn.IntType :: defn.LongType :: defn.ByteType :: defn.ShortType :: BigIntType :: Nil
        case FloatingPointXn => defn.DoubleType :: defn.FloatType :: BigDecimalType :: Nil
        case DateTimeXn      => defn.LongType :: CalendarType :: DateType :: Nil
        case LiteralXn       => Nil
        case ErrorXn         => Nil

    // what flags does the conversion accept?
    private def okFlags: String =
      kind match
        case StringXn => "-#<"
        case BooleanXn | HashXn => "-<"
        case LiteralXn => "-"
        case _ => "-#+ 0,(<"

    def hasFlag(f: Char) = flags.contains(f)
    def hasAnyFlag(fs: String) = fs.exists(hasFlag)

    def badFlag(f: Char, msg: String) =
      val i = flags.indexOf(f) match { case -1 => 0 case j => j }
      errorAt(Flags, i)(msg)

    def errorAt(g: SpecGroup, i: Int = 0)(msg: String)   = report.partError(msg, argi, descriptor.offset(g, i), descriptor.end(g))
    def warningAt(g: SpecGroup, i: Int = 0)(msg: String) = report.partWarning(msg, argi, descriptor.offset(g, i), descriptor.end(g))

  object Conversion:
    def apply(m: Match, i: Int): Conversion =
      def kindOf(cc: Char) = cc match
        case 's' | 'S' => StringXn
        case 'h' | 'H' => HashXn
        case 'b' | 'B' => BooleanXn
        case 'c' | 'C' => CharacterXn
        case 'd' | 'o' |
             'x' | 'X' => IntegralXn
        case 'e' | 'E' |
             'f' |
             'g' | 'G' |
             'a' | 'A' => FloatingPointXn
        case 't' | 'T' => DateTimeXn
        case '%' | 'n' => LiteralXn
        case _         => ErrorXn
      end kindOf
      m.group(CC) match
        case Some(cc) => new Conversion(m, i, kindOf(cc(0))).tap(_.verify)
        case None     => new Conversion(m, i, ErrorXn).tap(_.errorAt(Spec)(s"Missing conversion operator in '${m.matched}'; $literalHelp"))
    end apply
    val literalHelp = "use %% for literal %, %n for newline"
  end Conversion

  var reported = false

  private def partPosAt(index: Int, offset: Int, end: Int) =
    val pos = partsElems(index).sourcePos
    val bgn = pos.span.start + offset
    val fin = if end < 0 then pos.span.end else pos.span.start + end
    pos.withSpan(Span(bgn, fin, bgn))

  extension (r: report.type)
    def argError(message: String, index: Int): Unit = r.error(message, args(index).srcPos).tap(_ => reported = true)
    def partError(message: String, index: Int, offset: Int, end: Int = -1): Unit = r.error(message, partPosAt(index, offset, end)).tap(_ => reported = true)
    def partWarning(message: String, index: Int, offset: Int, end: Int = -1): Unit = r.warning(message, partPosAt(index, offset, end)).tap(_ => reported = true)
end TypedFormatChecker
