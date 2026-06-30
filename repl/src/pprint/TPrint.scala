package pprint

/**
  * Summoning an implicit `TPrint[T]` provides a pretty-printed
  * string representation of the type `T`, much better than is
  * provided by the default `Type#toString`. In particular
  *
  * - More forms are properly supported and printed
  * - Prefixed Types are printed un-qualified, according to
  *   what's currently in scope
  */
trait TPrint[T] {
  def render(implicit tpc: TPrintColors): fansi.Str

}

object TPrint extends TPrintLowPri{
  def recolor[T](s: fansi.Str): TPrint[T] = {
    new TPrint[T]{
      def render(implicit tpc: TPrintColors) = {
        val colors = s.getColors
        val updatedColors = colors.map{
          c => if (c == fansi.Color.Green.applyMask) tpc.typeColor.applyMask else 0L
        }
        fansi.Str.fromArrays(s.getChars, updatedColors)
      }
    }
  }
  def implicitly[T](implicit t: TPrint[T]): TPrint[T] = t
  implicit val NothingTPrint: TPrint[Nothing] =
    recolor[Nothing](fansi.Color.Green("Nothing"))

}

case class TPrintColors(typeColor: fansi.Attrs)

object TPrintColors {
  implicit object BlackWhite extends TPrintColors(fansi.Attrs())
  object Colors extends TPrintColors(fansi.Color.Green){
    implicit val Colored: TPrintColors = this
  }
}
