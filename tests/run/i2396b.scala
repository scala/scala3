class Bees {
  def f: PartialFunction[Bee, Unit] = {  case Bee(_) => ""  }

  f(new Bee("buzz"))

  case class Bee(value: String)
  //object Bee // With this it works
}

object Test {
  def main(args: Array[String]): Unit = {
    new Bees
  }
}
