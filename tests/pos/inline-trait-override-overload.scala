//> using options -language:experimental.inlineTraits
inline trait Foo[T]:
    def foo(x: T) = x
    def foo(x: String) = "Test"

class Bar extends Foo[Int]

@main def main =
    val x = Bar()
    println(x.foo(10)) 
    println(x.foo("Hello world")) 