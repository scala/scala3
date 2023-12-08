//> using options -language:experimental.erasedDefinitions

def foo(erased x: Int, y: Int) = y
def bar(x: Int, erased y: Int) = x

def consumeFoo(f: (erased x: Int, y: Int) => Int) = f(0, 1)

val fooF: (erased x: Int, y: Int) => Int = foo
val barF: (x: Int, erased y: Int) => Int = bar

val a = consumeFoo(foo) // ok
val b = consumeFoo(bar) // error

val c = consumeFoo(fooF) // ok
val d = consumeFoo(barF) // error
