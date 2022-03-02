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

object Test {

    def main(args: Array[String]): Unit = {
      summon[Mirror.ProductOf[Color]] // error
      summon[Mirror.SumOf[Color.Red.type]] // error
      summon[Mirror.SumOf[Color.Red.type | Color.Green.type]] // error
      summon[Mirror.SumOf[Bar.A | Bar.B]] // error
      summon[Mirror.SumOf[Singletons.A.type | Singletons.B.type]] // error
    }
}
