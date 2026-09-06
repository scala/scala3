//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized](x: T):
    def foo = x

def f(b: Foo[Int]) = 37 + b.foo
 
@main def main =
    val x = new Foo[Int](42) {}
    f(x)
