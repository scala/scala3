object Test2 {
  @main val x = 2  // error: @main annotation cannot be applied to value x
}

class Foo {
  @main def f = () // error: method f cannot be a @main method since it cannot be accessed statically
}

@main def g(x: Int*)(y: Int*) = () // error: varargs parameter of @main method must come last

@main def h[T: util.CommandLineParser.FromString](x: T) = () // error: @main method cannot have type parameters

@main def i(x: Int)(using Foo) = () // error: @main method cannot have implicit parameters
