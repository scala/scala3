import scala.util.NotGiven
import scala.compiletime.*
import scala.deriving.Mirror

enum Color:
  case Red, Green, Blue
end Color

object Test {

    def main(args: Array[String]): Unit = {
      summon[Mirror.ProductOf[Color]] // error
      summon[Mirror.SumOf[Color.Red.type]] // error
    }
}
