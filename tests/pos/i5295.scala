inline def foo: String = bar given (4)
private def bar: given Int => String = "baz"
