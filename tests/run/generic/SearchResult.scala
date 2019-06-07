package generic

import Shapes._

/** enum SearchResult {
 *    case Success(result: Color)
 *    case Diverging
 *    case NoMatch
 *    case Ambiguous(alt1: SearchResult, alt2: SearchResult)
 *  }
 */
sealed trait SearchResult extends Enum

object SearchResult {

  private val $values = new runtime.EnumValues[SearchResult]
  def valueOf = $values.fromInt
  def withName = $values.fromName
  def values = $values.values

  private def $new(tag: Int, name: String) = new SearchResult {
    def ordinal = tag
    override def toString = name
    $values.register(this)
  }

  abstract case class Success(result: Color) extends SearchResult {
    def ordinal = 0
  }
  object Success {
    def apply(result: Color): SearchResult = new Success(result) {}
    implicit def SuccessShape: Success `shaped` Color =
      new (Success `shaped` Color) {
        def toShape(s: Success) = s.result
        def fromShape(c: Color) = new Success(c) {}
    }
  }

  val Diverging: SearchResult = $new(1, "Diverging")
  val NoMatch: SearchResult = $new(2, "NoMatch")

  abstract case class Ambiguous(alt1: SearchResult, alt2: SearchResult) extends SearchResult {
    def ordinal = 3
  }
  object Ambiguous {
    def apply(alt1: SearchResult, alt2: SearchResult): SearchResult = new Ambiguous(alt1, alt2) {}
    implicit def AmbiguousShape: Ambiguous `shaped` Prod[SearchResult, SearchResult] =
      new (Ambiguous `shaped` Prod[SearchResult, SearchResult]) {
        def toShape(a: Ambiguous) = Prod(a.alt1, a.alt2)
        def fromShape(p: Prod[SearchResult, SearchResult]) = new Ambiguous(p.fst, p.snd) {}
    }
  }

  type Shape = Sum[Success, Sum[Ambiguous, EnumValue[SearchResult]]]

  implicit def SearchResultShape: SearchResult `unfolds` Shape =
    new (SearchResult `shaped` Shape) {
      def toShape(x: SearchResult) = x match {
        case x: Success => Fst(x)
        case x: Ambiguous => Snd(Fst(x))
        case x => Snd(Snd(EnumValue(x.ordinal)))
      }
      def fromShape(x: Sum[Success, Sum[Ambiguous, EnumValue[SearchResult]]]): SearchResult = x match {
        case Fst(s) => s
        case Snd(Fst(a)) => a
        case Snd(Snd(ev)) => valueOf(ev.tag)
      }
    }
}
