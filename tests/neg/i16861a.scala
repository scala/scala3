import scala.quoted.*
trait Foo
object Foo:
  inline given foo[T <: Foo]: T = summon[Type.of[T]] // error
