// scalajs: --skip

class Foo
object Foo extends Bar

trait Bar {
  def hi: Int = 1
  private def foo: Int = 1

  class Inner {
    val a = foo // Force foo to be expanded by ExpandPrivate
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println(
      classOf[Foo].getMethods
        .filter(m => (m.getModifiers & java.lang.reflect.Modifier.STATIC) != 0)
        .mkString("\n"))
  }
}
