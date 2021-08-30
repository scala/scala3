case class Context(x: Int)

class Test3()(using @transient ctx: Context) { // OK
  println(implicitly[Context])
}

class Test4(@transient private val x: Int) // OK

object Test:
  def main(args: Array[String]): Unit =
    given Context = Context(22)
    Test3()
