//> using options -language:experimental.specializedTraits
inline trait Foo[T: Specialized](x: T):
    def foo = x

trait Bar extends Foo[Int]: // error: Specialized traits may not be extended by ordinary traits. They may only be extended by classes, objects or inline/specialized traits.
    def myMethod = "Hello I am a method"

def f(b: Foo[Int]) = println(s"We found the following value of foo ${b.foo}")

@main def main =
    val x = new Bar with Foo(19) {}
    f(x)
