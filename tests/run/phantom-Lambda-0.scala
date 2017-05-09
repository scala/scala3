
object Test {
  import Boo._

  def main(args: Array[String]) = {
    (((b, b2) => println(42)): ((Pinky, Pinky) => Unit)).apply(pinky, pinky)
  }
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def pinky: Pinky = assume
}
