import annotation.retains

val foo: Int => Int = x => x
val bar: (Int -> Int) @retains[caps.any.type] = foo
val baz: Int => Int = bar


