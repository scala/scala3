class Foo[+X[_]] {
  // OK
  def foo1[Y[_]](right: Foo[Y]): Foo[Y] = right
  // OK
  def foo2[Y[_]](right: Foo[[T] =>> Y[T]]): Foo[Y] = right
  // OK
  def foo3[Y[_]](right: Foo[[T] =>> Y[T]]): Foo[[T] =>> Y[T]] = right
  // Error:
  //   found:    Foo[Y](right)
  //   required: Foo[Y]
  def foo4[Y[_]](right: Foo[Y]): Foo[[T] =>> Y[T]] = right
}
