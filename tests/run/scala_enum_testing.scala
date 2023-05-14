import scala.util.NotGiven
import scala.compiletime.*
import scala.deriving.Mirror

enum Color:
  case Red, Green, Blue
end Color

object Test {

    def main(args: Array[String]): Unit = {
      summon[Mirror.Of[Color.Red.type]]
      summon[Mirror.Of[Color]]
      summon[Mirror.ProductOf[Color.Red.type]]
      summon[Mirror.SumOf[Color]]
    }
}
