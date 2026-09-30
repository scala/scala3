//> using options -language:experimental.specializedTraits
inline trait Foo[T: Specialized](x: T):
    def foo = x

class Bar extends Foo(10):
    def myMethod = "Hello I am a method"

def f(b: Foo[Int]) = println(s"We found the following value of foo ${b.foo}")

@main def main =
    val x = Bar()
    f(x)
