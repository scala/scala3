package dotty.tools.dotc
package printing
import scala.annotation.internal.sharable

object Texts {

  @sharable
  private val ansi = java.util.regex.Pattern.compile("\u001b\\[\\d+m")

  sealed abstract class Text {

    protected def indentMargin: Int = 2

    def relems: List[Text]

    def isEmpty: Boolean = this match {
      case Str(s, _) => s.isEmpty
      case Fluid(relems) => relems forall (_.isEmpty)
      case Vertical(relems) => relems.isEmpty
    }

    //                 Str Ver Clo Flu
    // isVertical       F   T   F   F
    // isClosed         F   T   T   F
    // isFluid          F   F   T   T
    // isSplittable     F   F   F   T
    def isVertical: Boolean = isInstanceOf[Vertical]
    def isClosed: Boolean = isVertical || isInstanceOf[Closed]
    def isFluid: Boolean = isInstanceOf[Fluid]
    def isSplittable: Boolean = isFluid && !isClosed

    def close: Text = if isSplittable then Closed(relems) else this

    def remaining(width: Int): Int = this match {
      case Str(s, _) =>
        width - lengthWithoutAnsi(s)
      case Fluid(Nil) =>
        width
      case Fluid(last :: prevs) =>
        val r = last remaining width
        if (r < 0) r else Fluid(prevs) remaining r
      case Vertical(_) =>
        -1
    }

    def lastLine: String = this match {
      case Str(s, _) => s
      case _ => relems.head.lastLine
    }

    def appendToLastLine(that: Text): Text = that match {
      case Str(s2, lines1) =>
        this match {
          case Str(s1, lines2) => Str(s1 + s2, lines1 union lines2)
          case Fluid(Str(s1, lines2) :: prev) => Fluid(Str(s1 + s2, lines1 union lines2) :: prev)
          case Fluid(relems) => Fluid(that :: relems)
          case Vertical(_) => throw new IllegalArgumentException("Unexpected Vertical.appendToLastLine")
        }
      case Fluid(relems) =>
        relems.reverse.foldLeft(this)(_ appendToLastLine _)
      case Vertical(_) => throw new IllegalArgumentException("Unexpected Text.appendToLastLine(Vertical(...))")
    }

    private def appendIndented(that: Text)(width: Int): Text =
      Fluid(that.layout(width - indentMargin).indented :: this.relems)

    private def append(width: Int)(that: Text): Text =
      if (this.isEmpty) that.layout(width)
      else if (that.isEmpty) this
      else if (that.isVertical) appendIndented(that)(width)
      else if (this.isVertical) Fluid(that.layout(width) :: this.relems)
      else if (that.remaining(width - lengthWithoutAnsi(lastLine)) >= 0) appendToLastLine(that)
      else if (that.isSplittable) that.relems.reverse.foldLeft(this)(_.append(width)(_))
      else appendIndented(that)(width)

    private def lengthWithoutAnsi(str: String): Int =
      ansi.matcher(str).replaceAll("").length

    def layout(width: Int): Text = this match {
      case Str(s, _) =>
        this
      case Fluid(relems) =>
        relems.reverse.foldLeft(Str(""): Text)(_.append(width)(_))
      case Vertical(relems) =>
        Vertical(relems map (_ layout width))
    }

    def map(f: String => String): Text = this match {
      case Str(s, lines) => Str(f(s), lines)
      case Fluid(relems) => Fluid(relems map (_ map f))
      case Vertical(relems) => Vertical(relems map (_ map f))
    }

    def stripPrefix(pre: String): Text = this match {
      case Str(s, _) =>
        if (s.startsWith(pre)) s drop pre.length else s
      case Fluid(relems) =>
        val elems = relems.reverse
        val head = elems.head.stripPrefix(pre)
        if (head eq elems.head) this else Fluid((head :: elems.tail).reverse)
      case Vertical(relems) =>
        val elems = relems.reverse
        val head = elems.head.stripPrefix(pre)
        if (head eq elems.head) this else Vertical((head :: elems.tail).reverse)
    }

    private def indented: Text = this match {
      case Str(s, lines) => Str((" " * indentMargin) + s, lines)
      case Fluid(relems) => Fluid(relems map (_.indented))
      case Vertical(relems) => Vertical(relems map (_.indented))
    }

    def print(sb: StringBuilder, numberWidth: Int): Unit = this match {
      case Str(s, lines) =>
        if (numberWidth != 0) {
          val ln = lines.show
          if (ln.nonEmpty) {
            val pad = (numberWidth - ln.length - 1)
            assert(pad >= 0)
            sb.append(" " * pad)
            sb.append(ln)
            sb.append("|")
          }
        }
        sb.append(s.replaceAll("[ ]+$", ""))
      case _ =>
        var follow = false
        for (elem <- relems.reverse) {
          if (follow) sb.append(System.lineSeparator)
          elem.print(sb, numberWidth)
          follow = true
        }
    }

    def maxLine: Int = this match {
      case Str(_, lines) => lines.end
      case _ => relems.foldLeft(-1)((acc, relem) => acc max relem.maxLine)
    }

    def mkString(width: Int = Int.MaxValue, withLineNumbers: Boolean = false): String = {
      val sb = new StringBuilder
      // width display can be upto a range "n-n" where 1 <= n <= maxLine+1
      val numberWidth = if (withLineNumbers) (2 * (maxLine + 1).toString.length) + 2 else 0
      layout(width - numberWidth).print(sb, numberWidth)
      sb.toString
    }

    def ~ (that: Text): Text =
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else this match
        case Fluid(relems1) if !isClosed => that match
          case Fluid(relems2) if !that.isClosed => Fluid(relems2 ++ relems1)
          case _                                => Fluid(that +: relems1)
        case _             => that match
          case Fluid(relems2) if !that.isClosed => Fluid(relems2 :+ this)
          case _                                => Fluid(that :: this :: Nil)

    def ~~ (that: Text): Text =
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else Fluid(that :: Str(" ") :: this :: Nil)

    def over (that: Text): Vertical =
      if (this.isVertical) Vertical(that :: this.relems)
      else Vertical(that :: this :: Nil)
  }

  object Text {

    /** The empty text */
    def apply(): Text = Str("")

    /** A concatenation of elements in `xs` and interspersed with
     *  separator strings `sep`.
     */
    def apply(xs: Iterable[Text], sep: String = " "): Text =
      if (sep == "\n") lines(xs)
      else {
        val ys = xs.filterNot(_.isEmpty)
        if (ys.isEmpty) Str("")
        else ys.reduceRight((a, b) => (a ~ sep).close ~ b)
      }

    /** The given texts `xs`, each on a separate line */
    def lines(xs: Iterable[Text]): Vertical = Vertical(xs.toList.reverse)

    extension (text: => Text)
      def provided(cond: Boolean): Text = if (cond) text else Str("")

  }

  case class Str(s: String, lineRange: LineRange = EmptyLineRange) extends Text {
    override def relems: List[Text] = List(this)
    override def toString = this match
      case Str(s, EmptyLineRange) => s"Str($s)"
      case Str(s, lineRange)      => s"Str($s, $lineRange)"
  }

  case class Vertical(relems: List[Text]) extends Text
  case class Fluid(relems: List[Text]) extends Text

  class Closed(relems: List[Text]) extends Fluid(relems):
    override def productPrefix = "Closed"

  implicit def stringToText(s: String): Text = Str(s)

  /** Inclusive line range */
  case class LineRange(start: Int, end: Int) {
    def union(that: LineRange): LineRange = LineRange(start min that.start, end max that.end)
    def show: String =
      if (start == end) (start + 1).toString
      else if (start < end) s"${start + 1}-${end + 1}"
      else "" // empty range
  }

  object EmptyLineRange extends LineRange(Int.MaxValue, Int.MinValue)
}
