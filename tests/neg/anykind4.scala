
object Test {

  trait Foo[T <: AnyKind]

  def foo[T <: AnyKind](implicit f: Foo[T]): f.type = f

  implicit def c1[T]: Foo[T] = ???
  implicit def c2[T[_]]: Foo[T] = ???

  foo[List](c1) // error

  foo[List](c2)
}