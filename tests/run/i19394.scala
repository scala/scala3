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
    assert(Bar.bar == "bar")

    import scala.reflect.Selectable.reflectiveSelectable
    assert(Foo.asInstanceOf[{ def foo: String }].foo == "foo")
    assert(Bar.asInstanceOf[{ def bar: String }].bar == "bar")
