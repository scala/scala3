inline def foo: String = bar(using 4)
private def bar: Int ?=> String = "baz"
