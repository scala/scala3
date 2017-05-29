class B {
  val buzz = Some(Bees.Bee("buzz")).collect {
    case Bees.Bee(value) => value
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new B
  }
}

object Bees {
  case class Bee(value: String)
}
