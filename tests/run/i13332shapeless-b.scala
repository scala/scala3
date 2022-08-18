def outer3Local = {
  class Wrapper {
    object Nested {
      sealed trait Color
    }
  }
  val wrapper = new Wrapper
  import wrapper.Nested.Color

  object Inner {
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color
    case class Rgb(hex: Int) extends Color
    case object Rgb // force anonymous mirror for Rgb
  }

  import Inner.*
  val M = summon[deriving.Mirror.Of[Color]]
  assert(M.ordinal(Red) == 0)
  assert(M.ordinal(Green) == 1)
  assert(M.ordinal(Blue) == 2)
  assert(M.ordinal(Rgb(0xffaaff)) == 3)

  val M_Rgb =
    type TRgb = Tuple.Elem[M.MirroredElemTypes, 3]
    summon[deriving.Mirror.Of[TRgb]]

  assert(M_Rgb.fromProduct(Tuple(0xffaaff)) == Rgb(0xffaaff))
}

@main def Test =
  outer3Local
