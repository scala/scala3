package dotty.tools.dotc.util

import language.implicitConversions

object Texts {

  class Text {

    protected def indentMargin = 2
    protected def pageWidth = 80

    def relems: List[Text]

    def isEmpty: Boolean = this match {
      case Str(s) => s.isEmpty
      case Fluid(relems) => relems forall (_.isEmpty)
      case Rigid(relems) => relems.isEmpty
    }

    def isRigid = isInstanceOf[Rigid]

    def fitsIn(width: Int): Boolean = this match {
      case Str(s) =>
        s.length <= width
      case Fluid(Str(s) :: prevs) =>
        s.length < width && Fluid(prevs).fitsIn(width - s.length)
      case Rigid(_) =>
        false
    }

    def lastLineWidth: Int = this match {
      case Str(s) => s.length
      case _ => relems.head.lastLineWidth
    }

    def appendToLastLine(that: Text): Text = that match {
      case Str(s2) =>
        this match {
          case Str(s1) => Str(s1 + s2)
          case Fluid(Str(s1) :: prev) => Fluid(Str(s1 + s2) :: prev)
        }
      case Fluid(relems) =>
        (this /: relems.reverse)(_ appendToLastLine _)
    }

    private def appendIndented(that: Text)(width: Int): Text =
      Rigid(that.layout(width - indentMargin).indented :: this.relems)

    private def append(width: Int)(that: Text): Text =
      if (this.isEmpty) that.layout(width)
      else if (that.isEmpty) this
      else if (that.isRigid) appendIndented(that)(width)
      else if (this.isRigid) Fluid(that.layout(width) :: this.relems)
      else if (that.fitsIn(width - this.lastLineWidth)) appendToLastLine(that)
      else appendIndented(that)(width)

    def layout(width: Int): Text = this match {
      case Str(_) =>
        this
      case Fluid(relems) =>
        ((Str(""): Text) /: relems.reverse)(_.append(width)(_))
      case Rigid(relems) =>
        Rigid(relems map (_ layout width))
    }

    private def indented: Text = this match {
      case Str(s) => Str((" " * indentMargin) + s)
      case Fluid(relems) => Fluid(relems map (_.indented))
      case Rigid(relems) => Rigid(relems map (_.indented))
    }

    def print(sb: StringBuilder): Unit = this match {
      case Str(s) =>
        sb.append(s)
      case txt: CompoundText =>
        var follow = false
        for (elem <- txt.relems.reverse) {
          if (follow) sb.append("\n")
          elem.print(sb)
          follow = true
        }
    }

    def show: String =
      layout(pageWidth).showRaw

    def showRaw: String = {
      val sb = new StringBuilder
      print(sb)
      sb.toString
    }

    def +++ (that: Text) =
      if (this.isEmpty) that
      else if (that.isEmpty) this
      else Fluid(that :: this :: Nil)

    def over (that: Text) =
      if (this.isRigid) Rigid(that :: this.relems)
      else Rigid(that :: this :: Nil)
  }

  object Text {
    def apply(xs: Traversable[Text], sep: String): Text = {
      val ys = xs filterNot (_.isEmpty)
      if (ys.isEmpty) Str("")
      else ys reduce (_ +++ sep +++ _)
    }
  }

  abstract class CompoundText extends Text {
    def relems: List[Text]
  }

  case class Str(s: String) extends Text {
    override def relems: List[Text] = List(this)
  }

  case class Rigid(relems: List[Text]) extends CompoundText
  case class Fluid(relems: List[Text]) extends CompoundText

  implicit def stringToText(s: String): Text = Str(s)
}