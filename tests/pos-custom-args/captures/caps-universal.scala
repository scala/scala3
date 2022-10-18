import annotation.retainsUniversal

val foo: Int => Int = x => x
val bar: (Int -> Int) @retainsUniversal = foo
val baz: {*} Int -> Int = bar


