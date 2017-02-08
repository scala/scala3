package generic

object Shapes {

  trait Sum[+S1, +S2]
  case class Fst[+F](x: F) extends Sum[F, Nothing]
  case class Snd[+S](x: S) extends Sum[Nothing, S]

  case class Prod[+P1, +P2](fst: P1, snd: P2)

  case class Singleton[SI](value: SI)

  case class EnumValue[E](tag: Int)

  trait shaped[SH1, SH2] extends unfolds[SH1, SH2]

  trait unfolds[UN1, UN2] {
    def toShape(x: UN1): UN2
    def fromShape(x: UN2): UN1
  }
}

