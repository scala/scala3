object Test1 {
  @main def f(x: Foo) = () // error: no implicit argument of type util.FromString[Foo] was found
}

object Test2 {
  @main val x = 2  // does nothing, should this be made an error?
}

class Foo {
  @main def f = () // does nothing, should this be made an error?
}

@main def g(x: Int*)(y: Int*) = () // error: varargs parameter of @main method must come last

@main def h[T: util.FromString](x: T) = () // error: @main method cannot have type parameters
