def foo(x: Int, erased y: Int): Int = x
def bar(erased x: Int, y: Int): Int = y

val fooF: (x: Int, erased y: Int) => Int = foo

val fooG: (erased x: Int, y: Int) => Int = foo // error

val barF: (x: Int, erased y: Int) => Int = bar // error

val barG: (erased x: Int, y: Int) => Int = bar

