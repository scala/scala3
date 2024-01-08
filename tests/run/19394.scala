import scala.annotation.{ static, targetName }

class Foo
object Foo:
  @static def foo: String = "foo"
  @targetName("foo") def fooBincompat: String = foo

class Bar
object Bar:
  @static def bar: String = "bar"
  def bar: String = bar

object Test:
  def main(args: Array[String]): Unit =
    assert(Foo.foo == "foo")
    assert(classOf[Foo].getMethod("foo").invoke(null) == "foo") // static
    assert(Foo.getClass.getMethod("foo").invoke(Foo) == "foo") // instance, on module class

    assert(Bar.bar == "bar")
    assert(classOf[Bar].getMethod("bar").invoke(null) == "bar")
    assert(Bar.getClass.getMethod("bar").invoke(Bar) == "bar")
