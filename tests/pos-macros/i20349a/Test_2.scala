
class Cls{
  object a {
    object domain {
      val value = ""
    }
  }
  Macros.values[a.domain.type]
}

object Test {
  lazy val script = new Cls()
  def main(args: Array[String]): Unit =
    val _ = script.hashCode()
    ???
}
