package dotty.tools.dotc
package printing
import core.Contexts.Context
import language.implicitConversions

object Texts {

  abstract class Text {

    protected def indentMargin = 2

    def relems: List[Text]

    def isEmpty: Boolean = this match {
      case Str(s) => s.isEmpty
      case Fluid(relems) => relems forall (_.isEmpty)
      case Vertical(relems) => relems.isEmpty
    }

    def isVertical = isInstanceOf[Vertical]
    def isClosed = isVertical || isInstanceOf[Closed]
    def isFluid = isInstanceOf[Fluid]
    def isSplittable = isFluid && !isClosed

    def close = new Closed(relems)

    def remaining(width: Int): Int = this match {
      case Str(s) =>
        width - s.length
      case Fluid(Nil) =>
        width
      case Fluid(last :: prevs) =>
        val r = last remaining width
        if (r < 0) r else Fluid(prevs) remaining r
      case Vertical(_) =>
        -1
    }

    def lastLine: String = this match {
      case Str(s) => s
      case _ => relems.head.lastLine
    }

    def appendToLastLine(that: Text): Text = that match {
      case Str(s2) =>
        this match {
          case Str(s1) => Str(s1 + s2)
          case Fluid(Str(s1) :: prev) => Fluid(Str(s1 + s2) :: prev)
          case Fluid(relems) => Fluid(that :: relems)
        }
      case Fluid(relems) =>
        (this /: relems.reverse)(_ appendToLastLine _)
    }

    private def appendIndented(that: Text)(width: Int): Text =
      Vertical(that.layout(width - indentMargin).indented :: this.relems)

    private def append(width: Int)(that: Text): Text = {
      if (this.isEmpty) that.layout(width)
      else if (that.isEmpty) this
      else if (that.isVertical) appendIndented(that)(width)
      else if (this.isVertical) Fluid(that.layout(width) :: this.relems)
      else if (that.remaining(width - lastLine.length) >= 0) appendToLastLine(that)
      else if (that.isSplittable) (this /: that.relems.reverse)(_.append(width)(_))
      else appendIndented(that)(width)
    }

    def layout(width: Int): Text = this match {
      case Str(_) =>
        this
      case Fluid(relems) =>
        ((Str(""): Text) /: relems.reverse)(_.append(width)(_))
      case Vertical(relems) =>
        Vertical(relems map (_ layout width))
    }

    def map(f: String => String): Text = this match {
      case Str(s) => Str(f(s))
      case Fluid(relems) => Fluid(relems map (_ map f))
      case Vertical(relems) => Vertical(relems map (_ map f))
    }

    def stripPrefix(pre: String): Text = this match {
      case Str(s) =>
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
      case Str(s) => Str((" " * indentMargin) + s)
      case Fluid(relems) => Fluid(relems map (_.indented))
      case Vertical(relems) => Vertical(relems map (_.indented))
    }

    def print(sb: StringBuilder): Unit = this match {
      case Str(s) =>
        sb.append(s)
      case _ =>
        var follow = false
        for (elem <- relems.reverse) {
          if (follow) sb.append("\n")
          elem.print(sb)
          follow = true
        }
    }

    def mkString(width: Int): String = {
      val sb = new StringBuilder
      layout(width).print(sb)
      sb.toString
    }

    def ~ (that: Text) =
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else Fluid(that :: this :: Nil)

    def ~~ (that: Text) =
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else Fluid(that :: Str(" ") :: this :: Nil)

    def over (that: Text) =
      if (this.isVertical) Vertical(that :: this.relems)
      else Vertical(that :: this :: Nil)

    def provided(pred: Boolean) = if (pred) this else Str("")
  }

  object Text {

    /** The empty text */
    def apply(): Text = Str("")

    /** A concatenation of elements in `xs` and interspersed with
     *  separator strings `sep`.
     */
    def apply(xs: Traversable[Text], sep: String = " "): Text = {
      if (sep == "\n") lines(xs)
      else {
        val ys = xs filterNot (_.isEmpty)
        if (ys.isEmpty) Str("")
        else ys reduce (_ ~ sep ~ _)
      }
    }

    /** The given texts `xs`, each on a separate line */
    def lines(xs: Traversable[Text]) = Vertical(xs.toList.reverse)
  }

  case class Str(s: String) extends Text {
    override def relems: List[Text] = List(this)
  }

  case class Vertical(relems: List[Text]) extends Text
  case class Fluid(relems: List[Text]) extends Text

  class Closed(relems: List[Text]) extends Fluid(relems)

  implicit def stringToText(s: String): Text = Str(s)
}
