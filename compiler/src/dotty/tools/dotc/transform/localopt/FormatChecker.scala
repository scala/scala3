package dotty.tools.dotc
package transform.localopt

import scala.annotation.tailrec
import scala.collection.mutable.{ListBuffer, Stack}
import scala.reflect.{ClassTag, classTag}
import scala.util.chaining.*
import scala.util.matching.Regex.Match

import java.util.{Calendar, Date, Formattable}

import PartialFunction.cond

/** Formatter string checker. */
abstract class FormatChecker(using reporter: InterpolationReporter):

  // Pick the first runtime type which the i'th arg can satisfy.
  // If conversion is required, implementation must emit it.
  def argType(argi: Int, types: ClassTag[?]*): ClassTag[?]

  // count of args, for checking indexes
  def argc: Int

  // match a conversion specifier
  val formatPattern = """%(?:(\d+)\$)?([-#+ 0,(<]+)?(\d+)?(\.\d+)?([tT]?[%a-zA-Z])?""".r

  // ordinal is the regex group index in the format pattern
  enum SpecGroup:
    case Spec, Index, Flags, Width, Precision, CC
  import SpecGroup.*

  /** For N part strings and N-1 args to interpolate, normalize parts and check arg types.
   *
   *  Returns parts, possibly updated with explicit leading "%s",
   *  and conversions for each arg.
   *
   *  Implementation must emit conversions required by invocations of `argType`.
   */
  def checked(parts0: List[String]): (List[String], List[Conversion]) =
    val amended = ListBuffer.empty[String]
    val convert = ListBuffer.empty[Conversion]

    @tailrec
    def loop(parts: List[String], n: Int): Unit =
      parts match
        case part0 :: more =>
          def badPart(t: Throwable): String = "".tap(_ => reporter.partError(t.getMessage, index = n, offset = 0))
          val part = try StringContext.processEscapes(part0) catch badPart
          val matches = formatPattern.findAllMatchIn(part)

          def insertStringConversion(): Unit =
            amended += "%s" + part
            convert += Conversion(formatPattern.findAllMatchIn("%s").next(), n)  // improve
            argType(n-1, classTag[Any])
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
              if cv.index.getOrElse(-1) == n then accept(cv)
              else
                // either some other arg num, or '<'
                //c.warning(op.groupPos(Index), "Index is not this arg")
                insertStringConversion()
            else if !cv.isError then accept(cv)

          // any remaining conversions in this part must be either literals or indexed
          while matches.hasNext do
            val cv = Conversion(matches.next(), n)
            if n == 0 && cv.hasFlag('<') then cv.badFlag('<', "No last arg")
            else if !cv.isLiteral && !cv.isIndexed then errorLeading(cv)

          loop(more, n + 1)
        case Nil => ()
    end loop

    loop(parts0, n = 0)
    (amended.toList, convert.toList)
  end checked

  extension (descriptor: Match)
    def at(g: SpecGroup): Int = descriptor.start(g.ordinal)
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
        case ErrorXn => '?'
        case DateTimeXn => if op.length > 1 then op(1) else '?'
        case _ => op(0)

    def isIndexed: Boolean = index.nonEmpty || hasFlag('<')
    def isError: Boolean   = kind == ErrorXn
    def isLiteral: Boolean = kind == LiteralXn

    // descriptor is at index 0 of the part string
    def isLeading: Boolean = descriptor.at(Spec) == 0

    // flags and index in specifier are ok
    private def goodies = goodFlags && goodIndex

    // true if passes. Default checks flags and index
    def verify: Boolean =
      kind match {
        case StringXn        => goodies
        case BooleanXn       => goodies
        case HashXn          => goodies
        case CharacterXn     => goodies && noPrecision && only_-("c conversion")
        case IntegralXn      =>
          def d_# = cc == 'd' && hasFlag('#') and badFlag('#', "# not allowed for d conversion")
          def x_comma = cc != 'd' && hasFlag(',') and badFlag(',', "',' only allowed for d conversion of integral types")
          goodies && noPrecision && !d_# && !x_comma
        case FloatingPointXn =>
          goodies && (cc match {
            case 'a' | 'A' =>
              val badFlags = ",(".filter(hasFlag)
              noPrecision && badFlags.isEmpty or badFlags.foreach(badf => badFlag(badf, s"'$badf' not allowed for a, A"))
            case _ => true
          })
        case DateTimeXn      =>
          def hasCC = op.length == 2 or errorAt(CC)("Date/time conversion must have two characters")
          def goodCC = "HIklMSLNpzZsQBbhAaCYyjmdeRTrDFc".contains(cc) or errorAt(CC, 1)(s"'$cc' doesn't seem to be a date or time conversion")
          goodies && hasCC && goodCC && noPrecision && only_-("date/time conversions")
        case LiteralXn       =>
          op match {
            case "%" => goodies && noPrecision and width.foreach(_ => warningAt(Width)("width ignored on literal"))
            case "n" => noFlags && noWidth && noPrecision
          }
        case ErrorXn         =>
          errorAt(CC)(s"illegal conversion character '$cc'")
          false
      }

    // is the specifier OK with the given arg
    def accepts(arg: ClassTag[?]): Boolean =
      kind match
        case BooleanXn  => arg == classTag[Boolean] orElse warningAt(CC)("Boolean format is null test for non-Boolean")
        case IntegralXn =>
          arg == classTag[BigInt] || !cond(cc) {
            case 'o' | 'x' | 'X' if hasAnyFlag("+ (") => "+ (".filter(hasFlag).foreach(bad => badFlag(bad, s"only use '$bad' for BigInt conversions to o, x, X")) ; true
          }
        case _ => true

    // what arg type if any does the conversion accept
    def acceptableVariants: List[ClassTag[?]] =
      kind match {
        case StringXn        => if hasFlag('#') then classTag[Formattable] :: Nil else classTag[Any] :: Nil
        case BooleanXn       => classTag[Boolean] :: Conversion.FakeNullTag :: Nil
        case HashXn          => classTag[Any] :: Nil
        case CharacterXn     => classTag[Char] :: classTag[Byte] :: classTag[Short] :: classTag[Int] :: Nil
        case IntegralXn      => classTag[Int] :: classTag[Long] :: classTag[Byte] :: classTag[Short] :: classTag[BigInt] :: Nil
        case FloatingPointXn => classTag[Double] :: classTag[Float] :: classTag[BigDecimal] :: Nil
        case DateTimeXn      => classTag[Long] :: classTag[Calendar] :: classTag[Date] :: Nil
        case LiteralXn       => Nil
        case ErrorXn         => Nil
      }

    // what flags does the conversion accept?
    private def okFlags: String =
      kind match {
        case StringXn => "-#<"
        case BooleanXn | HashXn => "-<"
        case LiteralXn => "-"
        case _ => "-#+ 0,(<"
      }

    def hasFlag(f: Char) = flags.contains(f)
    def hasAnyFlag(fs: String) = fs.exists(hasFlag)

    def badFlag(f: Char, msg: String) =
      val i = flags.indexOf(f) match { case -1 => 0 case j => j }
      errorAt(Flags, i)(msg)

    def errorAt(g: SpecGroup, i: Int = 0)(msg: String)   = reporter.partError(msg, argi, descriptor.offset(g, i))
    def warningAt(g: SpecGroup, i: Int = 0)(msg: String) = reporter.partWarning(msg, argi, descriptor.offset(g, i))

    // various assertions
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
    private val FakeNullTag: ClassTag[?] = null
  end Conversion
