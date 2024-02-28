package lib

object Wrap:
  def foo[T: Numeric]: String ?=> T =
    summon[Numeric[T]].fromInt(summon[String].length)