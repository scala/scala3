case class Pig(id: Int, luckyNumber: Int)

case class Horse(id: Int)
case class PigWithSomeHorse(id: Int, luckyNumber: Int, favouriteHorse: Option[Horse])

class Main:
    def main: Unit = {
        val snowball = Pig(1, 2)
        val snowball1 = Pig(1, 2)

        if(snowball == snowball1){
            println("Good job!")
        }

        val clover = Horse(10)
        val napoleon = PigWithSomeHorse(1, 2, Some(clover))
        val napoleon1 = PigWithSomeHorse(1, 2, Some(clover))

        if(napoleon == napoleon1){
            println("Good job Value Option!")
        }
    }