import scala.annotation.*

trait X

object Foo:
  def foo(using @implicitNotFound("foo!") x: X) = "foo"

object FooClone:
  export Foo.foo

object Main:
  val n = 10
  Foo.foo        // error
  FooClone.foo   // error