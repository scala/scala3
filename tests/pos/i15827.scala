
trait Mirr {
  type MirroredTp
  type Elems <: Tuple
}
trait MirrP extends Mirr {
  def fromProduct(x: Product): MirroredTp
}
trait MirrS extends Mirr

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
    case object Rgb
  }

  object CallSite {
    def run =
      import Inner.*
      val M: (MirrS { type MirroredTp = Color; type Elems = (Inner.Red.type, Inner.Green.type, Inner.Blue.type, Inner.Rgb) }) =
        new MirrS {
          type MirroredTp = Color
          type Elems = (Inner.Red.type, Inner.Green.type, Inner.Blue.type, Inner.Rgb)
        }

      val M_Rgb =
        type TRgb = Tuple.Elem[M.Elems, 3]
        new MirrP {
          type MirroredTp = TRgb
          type Elems = Int *: EmptyTuple

          def fromProduct(x: Product): MirroredTp =
            new TRgb(x.productElement(0).asInstanceOf[Int])
        }: (MirrP {
          type MirroredTp = TRgb
          type Elems = Int *: EmptyTuple
        })
  }

  CallSite.run
}

@main def Test =
  outer3Local
