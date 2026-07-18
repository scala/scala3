import language.experimental.pureFunctions
import annotation.retains

val id: Int -> Int = (x: Int) => x
val foo: Int => Int = id
val bar: (Int -> Int) @retains[caps.any.type] = foo




