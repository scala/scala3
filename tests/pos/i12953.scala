class Schema(impl: Class[?]) extends scala.annotation.StaticAnnotation

class Ann[A] extends scala.annotation.StaticAnnotation

case class Foo[A](@Schema(classOf[List[A]]) foo: String)
case class Bar[A](@Ann[A] foo: String)
def baz[A](@Ann[A] foo: String) = ()
