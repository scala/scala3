import scala.language.`3.1-migration`
import scala.reflect.TypeTest

trait Foo with
  type X
  type Y <: X
  def x: X
  given TypeTest[X, Y] = ???

object Test with
  def test(foo: Foo): Unit =
    foo.x match
      case x: foo.Y =>
      case x: foo.X =>
