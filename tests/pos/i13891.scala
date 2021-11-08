trait TC[A] {
  def cast(any: Any): A
}
object TC {
  given TC[Int] with {
    override def cast(any: Any): Int = any.asInstanceOf[Int]
  }
  given TC[String] with {
    override def cast(any: Any): String = any.asInstanceOf[String]
  }
}

class DiscordRecord(private val map: Map[String, Any]) extends Selectable {
  def selectDynamic[A](name: String)(using decoder: TC[A]): A =
    decoder.cast(map(name))
}

type Test = DiscordRecord {
  val foo: Int
}

val t: Test = ???
def test = t.foo
