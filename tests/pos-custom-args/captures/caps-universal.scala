import annotation.retains

val foo: Int => Int = x => x
val bar: (Int -> Int) @retains(caps.*) = foo
val baz: {*} Int -> Int = bar


