
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

    (new Function2[Blinky, Blinky, Unit] {
      def apply(p1: Blinky, p2: Blinky) = println("AnnonPhantomsFunction1.apply() 3")
    }).apply(boo[Blinky], boo[Blinky])

  }
}

object Boo extends Phantom {
  type Blinky <: this.Any
  def boo[B <: Blinky]: B = assume
}
