
object Test {
  import Boo._

  def main(args: Array[String]) = {
    (((b, i) => println(i)): ((Pinky, Int) => Unit)).apply(pinky, 42)
  }
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def pinky: Pinky = assume
}
