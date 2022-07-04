// scalajs: --skip

object Foo
object Test {
  def main(args: Array[String]) =
    assert(Foo.getClass.getConstructors.length == 0)
}