package dotty.tools.dotc
package printing
import scala.annotation.internal.sharable

object Texts {

  @sharable
  private val ansi = java.util.regex.Pattern.compile("\u001b\\[\\d+m")

  private val indentMargin: Int = 2

  sealed abstract class Text {

    /** The sub-elements of this text in reverse order */
    def relems: List[Text]

    def isEmpty: Boolean = this match
      case Str(s) => s.isEmpty
      case Fluid(relems) => relems forall (_.isEmpty)
      case Vertical(relems) => relems.isEmpty

    //                 Str Ver Clo Flu
    // isVertical       F   T   F   F
    // isClosed         F   T   T   F
    // isFluid          F   F   T   T
    // isSplittable     F   F   F   T
    def isVertical: Boolean = isInstanceOf[Vertical]
    def isClosed: Boolean = isVertical || isInstanceOf[Closed]
    def isFluid: Boolean = isInstanceOf[Fluid]
    def isSplittable: Boolean = isFluid && !isClosed

    def close: Text =
      if isSplittable then Closed(relems) else this

    def remaining(width: Int): Int = this match
      case Str(s) =>
        width - lengthWithoutAnsi(s)
      case Fluid(Nil) =>
        width
      case Fluid(last :: prevs) =>
        val r = last.remaining(width)
        if (r < 0) r else Fluid(prevs).remaining(r)
      case Vertical(_) =>
        -1

    def lastLine: String = this match
      case Str(s) => s
      case _ => relems.head.lastLine

    def appendToLastLine(that: Text): Text = that match
      case Str(s2) =>
        this match
          case Str(s1) => Str(s1 + s2)
          case Fluid(Str(s1) :: prev) => Fluid(Str(s1 + s2) :: prev)
          case Fluid(relems) => Fluid(that :: relems)
          case Vertical(_) => throw new IllegalArgumentException("Unexpected Vertical.appendToLastLine")
      case Fluid(relems) =>
        relems.reverse.foldLeft(this)(_.appendToLastLine(_))
      case Vertical(_) => throw new IllegalArgumentException("Unexpected Text.appendToLastLine(Vertical(...))")

    private def appendIndented(that: Text)(width: Int): Text =
      Fluid(that.layout(width - indentMargin).indented :: this.relems)

    private def append(width: Int)(that: Text): Text =
      if this.isEmpty then that.layout(width)
      else if that.isEmpty then this
      else if that.isVertical then appendIndented(that)(width)
      else if this.isVertical then Fluid(that.layout(width) :: this.relems)
      else if that.remaining(width - lengthWithoutAnsi(lastLine)) >= 0 then appendToLastLine(that)
      else if that.isSplittable then that.relems.reverse.foldLeft(this)(_.append(width)(_))
      else appendIndented(that)(width)

    private def lengthWithoutAnsi(str: String): Int =
      ansi.matcher(str).replaceAll("").length

    def layout(width: Int): Text = this match
      case Str(s) =>
        this
      case Fluid(relems) =>
        relems.reverse.foldLeft(Str(""): Text)(_.append(width)(_))
      case Vertical(relems) =>
        Vertical(relems.map(_.layout(width)))

    def map(f: String => String): Text = this match
      case Str(s) => Str(f(s))
      case Fluid(relems) => Fluid(relems.map(_.map(f)))
      case Vertical(relems) => Vertical(relems.map(_.map(f)))

    def stripPrefix(pre: String): Text = this match
      case Str(s) =>
        if s.startsWith(pre) then s.drop(pre.length) else s
      case Fluid(relems) =>
        val elems = relems.reverse
        val head = elems.head.stripPrefix(pre)
        if head eq elems.head then this else Fluid((head :: elems.tail).reverse)
      case Vertical(relems) =>
        val elems = relems.reverse
        val head = elems.head.stripPrefix(pre)
        if (head eq elems.head) this else Vertical((head :: elems.tail).reverse)

    private def indented: Text = this match
      case Str(s) => Str((" " * indentMargin) + s)
      case Fluid(relems) => Fluid(relems map (_.indented))
      case Vertical(relems) => Vertical(relems map (_.indented))

    def print(sb: StringBuilder): Unit = this match
      case Str(s) =>
        sb.append(s.replaceAll("[ ]+$", ""))
      case _ =>
        var follow = false
        for elem <- relems.reverse do
          if follow then sb.append(System.lineSeparator)
          elem.print(sb)
          follow = true

    def mkString(width: Int = Int.MaxValue): String =
      val sb = new StringBuilder
      // width display can be upto a range "n-n" where 1 <= n <= maxLine+1
      layout(width).print(sb)
      sb.toString

    def ~ (that: Text): Text =
      if this.isEmpty then that
      else if that.isEmpty then this
      else this match
        case Fluid(relems1) if !isClosed => that match
          case Fluid(relems2) if !that.isClosed => Fluid(relems2 ++ relems1)
          case _                                => Fluid(that +: relems1)
        case _             => that match
          case Fluid(relems2) if !that.isClosed => Fluid(relems2 :+ this)
          case _                                => Fluid(that :: this :: Nil)

    def ~~ (that: Text): Text =
      if this.isEmpty then that
      else if that.isEmpty then this
      else Fluid(that :: Str(" ") :: this :: Nil)

    def over (that: Text): Vertical =
      if this.isVertical then Vertical(that :: this.relems)
      else Vertical(that :: this :: Nil)
  }

  object Text {

    /** The empty text */
    def apply(): Text = Str("")

    /** A concatenation of elements in `xs` and interspersed with
     *  separator strings `sep`.
     */
    def apply(xs: Iterable[Text], sep: String = " "): Text =
      if sep == "\n" then lines(xs)
      else
        val ys = xs.filterNot(_.isEmpty)
        if ys.isEmpty then Str("")
        else ys.reduceRight((a, b) => (a ~ sep).close ~ b)

    /** The given texts `xs`, each on a separate line */
    def lines(xs: Iterable[Text]): Vertical = Vertical(xs.toList.reverse)

    extension (text: => Text)
      def provided(cond: Boolean): Text = if (cond) text else Str("")
  }

  case class Str(s: String) extends Text:
    override def relems: List[Text] = List(this)
    override def toString = s"Str($s)"

  case class Vertical(relems: List[Text]) extends Text
  case class Fluid(relems: List[Text]) extends Text

  class Closed(relems: List[Text]) extends Fluid(relems):
    override def productPrefix = "Closed"

  given Conversion[String, Text] = Str(_)
}
