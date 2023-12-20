import scala.annotation.static
trait Foo[A]
object Foo:
  @static val foo: Foo[String] = new {}
  @static given Foo[String] = foo

  def bar =
    val foo: Foo[String] = new {}
    given Foo[String] = foo
