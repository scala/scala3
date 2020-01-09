class Test {
  class Foo[+T](val x: T)

  def unfoo[T](foo: Foo[T]): T = foo.x

  def fooArray: Foo[Array[Object]] = new Foo(Array.empty[Object])

  unfoo(fooArray) ++ unfoo(fooArray)
}
