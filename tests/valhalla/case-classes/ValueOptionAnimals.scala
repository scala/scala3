import valuelib.*
import scala.annotation.valhalla

@valhalla
case class Horse(id: Int) extends AnyVal with DeepValhalla

@valhalla
class NonCaseClassPig(val id: Int, val luckyNumber: Int) extends AnyVal

@valhalla
case class PigWithSomeHorse(id: Int, luckyNumber: Int, favouriteHorse: Option[Horse]) extends AnyVal

@valhalla
case class PigWithValueSomeHorse(id: Int, luckyNumber: Int, favouriteHorse: ValueOption[Horse]) extends AnyVal with DeepValhalla

class IdentityPig(val id: Int, val luckyNumber: Int)

object Test:
    def main(args: Array[String]): Unit = {
        val clover = Horse(10)
        val clover1 = Horse(10)
        val notClover = Horse(11)

        assert(clover == clover1 && clover != notClover) // deep valhalla case class should use acmp

        val oldMajor = NonCaseClassPig(1, 2)
        val oldMajor1 = NonCaseClassPig(1, 2)

        assert(oldMajor == oldMajor1) // valhalla class uses .equals()

        val napoleon = PigWithValueSomeHorse(1, 2, ValueSome(clover))
        val napoleon1 = PigWithValueSomeHorse(1, 2, ValueSome(clover))
        val napoleon2 = PigWithValueSomeHorse(1, 2, ValueSome(clover1))

        assert(napoleon == napoleon1 && napoleon == napoleon2) // deep valhalla all the way down should use acmp

        val snowball = PigWithSomeHorse(1, 2, Some(clover))
        val snowball1 = PigWithSomeHorse(1, 2, Some(clover))
        val snowball2 = PigWithSomeHorse(1, 2, Some(clover1))

        assert(snowball == snowball1 && snowball == snowball2) // case class .equals()
    }