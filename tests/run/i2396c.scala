class Bees {
  import Test._

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
