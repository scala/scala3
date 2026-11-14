def foo(x: Int)(y: String): Int = x

val bar: (Int, String) => Int = foo // error