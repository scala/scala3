import scala.util.NotGiven
import scala.compiletime.*
import scala.deriving.Mirror

enum Color:
  case Red, Green, Blue
end Color

enum Bar:
  case A(i: Int)
  case B(b: Boolean)
  case C(s: String)

object Singletons {
  object A
  object B
}

type SumOfK1[F[_]] = Mirror.Sum { type MirroredType[T] = F[T] }

class refined extends scala.annotation.RefiningAnnotation

object Test {

    def main(args: Array[String]): Unit = {
      summon[Mirror.ProductOf[Color]] // error
      summon[Mirror.SumOf[Color.Red.type]] // error
      summon[Mirror.SumOf[Color.Red.type | Color.Green.type]] // error
      summon[Mirror.SumOf[Bar.A | Bar.B]] // error
      summon[Mirror.SumOf[Singletons.A.type | Singletons.B.type]] // error
      summon[SumOfK1[[X] =>> Bar]]
      summon[SumOfK1[[X] =>> Bar.A | Bar.B]] // error
      summon[SumOfK1[[X] =>> (Bar.A | Bar.B) @refined]] // error
      object opaques {
        opaque type Query[X] = (Bar.A | Bar.B) @refined
      }
      summon[SumOfK1[opaques.Query]] // error
      summon[SumOfK1[[X] =>> (Bar.A @refined) | Bar.B]] // error
    }
}
