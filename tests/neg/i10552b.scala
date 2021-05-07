class Foo:
  transparent inline def foo1[A <: Int]: Int = valueOf[A]
  transparent inline def foo2[A >: Int <: Int]: Int = valueOf[A]
  transparent inline def foo3[A]: Int = ???

  type X >: AnyKind <: AnyKind

  def run =
    println(foo1[Int])
    println(foo1["hi"]) // error
    println(foo1[String]) // error
    println(foo1[Any]) // error
    println(foo1[AnyKind]) // error

    println(foo2["hi"]) // error

    println(foo3[X]) // error
