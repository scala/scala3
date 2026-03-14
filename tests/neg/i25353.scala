
type Foo = Foo.Type
object Foo:
  opaque type Type = String

  def size(x: Foo): Int =
    implicitly[Foo =:= String]       // ok
    x.length                         // ok

  def size2(x: Type): Int =
    x.length // ok

object Bar:
  def size(x: Foo): Int =
    implicitly[Foo =:= String]       // error
    x.length                         // error

  def size2(x: Foo.Type): Int =
    x.length // error