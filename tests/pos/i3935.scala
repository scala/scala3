enum Foo3[T](x: T) {
  case Bar[S, T](y: T) extends Foo3[y.type](y)
}

val foo: Foo3.Bar[Nothing, 3] = Foo3.Bar(3)
val bar = foo

def baz[T](f: Foo3[T]): f.type = f

val qux = baz(bar) // existentials are back in Dotty?
