import scala.language.dynamics

class DynImpl extends Dynamic {
  def applyDynamic(name: String)(args: Any*)(implicit implicitArg: String = "foo"): String =
    s"method '$name' called with arguments ${args.mkString("'", "', '", "'")} and implicit argument '$implicitArg'"
}

object Test {
  def main(args:Array[String]): Unit = {
    val d = new DynImpl

    println(d.some())

    println(d.ints(1, 2, 3))

    println(d.strings("a", "b", "c"))
  }
}
