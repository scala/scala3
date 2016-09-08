
object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    val anonPhantomsFunction1 = new Function1[Blinky, Unit] {
      def apply(p1: Blinky) = println("AnnonPhantomsFunction1.apply() 1")
    }
    anonPhantomsFunction1(boo[Blinky])

    (new Function1[Blinky, Unit] {
      def apply(p1: Blinky) = println("AnnonPhantomsFunction1.apply() 2")
    }).apply(boo[Blinky])

    (new Function2[Blinky, Int, Unit] {
      def apply(p1: Blinky, i: Int) = println("AnnonPhantomsFunction1.apply() " + i)
    }).apply(boo[Blinky], 3)

  }
}

object Boo extends Phantom {
  type Blinky <: this.Any
  def boo[B <: Blinky]: B = assume[B]
}
