inline def foo: String = bar(given 4)
private def bar: Int ?=> String = "baz"
