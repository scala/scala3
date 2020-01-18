inline def foo: String = bar.with(4)
private def bar: Int ?=> String = "baz"
