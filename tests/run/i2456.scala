class Bees {
  def f: PartialFunction[Bee, Unit] = {  case Bee(_) => println("buzzzzzzz")  }

  f(new Bee("buzz"))

  case class Bee(value: String)
}

object Test {
  def main(args: Array[String]): Unit = {
    new Bees
  }
}
