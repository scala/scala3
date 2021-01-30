class Bees {
  import Test.*

  def f: PartialFunction[Bee, Unit] = {
     case Test.Bee(_) => ""
 //    case Bee(_) => ""  // This one works
  }

  f(new Bee("buzz"))
}

object Test {
  case class Bee(value: String)
  def main(args: Array[String]): Unit = new Bees
}
