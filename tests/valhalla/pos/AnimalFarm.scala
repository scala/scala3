import scala.annotation.valhalla

@valhalla
case class Pig(id: Int, luckyNumber: Int) extends AnyVal

@valhalla
case class Horse(id: Int) extends AnyVal

@valhalla
case class PigWithSomeHorse(id: Int, luckyNumber: Int, favouriteHorse: Option[Horse]) extends AnyVal

class Main:
    def main: Unit = {
        val snowball = Pig(1, 2)
        val snowball1 = Pig(1, 2)
        val notSnowball = Pig(1, 3)

        if(snowball == snowball1 && snowball != notSnowball){
            println("Good job!")
        }

        val clover = Horse(10)
        val unnamedHorse1 = Horse(10)
        val napoleon = PigWithSomeHorse(1, 2, Some(clover))
        val napoleon1 = PigWithSomeHorse(1, 2, Some(clover))
        val napoleon2 = PigWithSomeHorse(1, 2, Some(unnamedHorse1))

        if(napoleon == napoleon1 && napoleon == napoleon2){
            println("Good job! We are using case class equals!")
        }
    }