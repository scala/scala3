// scalajs: --skip
// (this special case is JVM-only)

object Foo
def foo: Any = Foo

object Test:
  def main(args: Array[String]): Unit =
    println(Foo.isInstanceOf[Serializable]) // should not be constant-folded to false, at runtime it is on the JVM
    println(foo.isInstanceOf[Serializable])
