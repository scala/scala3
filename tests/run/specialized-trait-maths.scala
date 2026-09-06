//> using options -language:experimental.specializedTraits

inline trait Foo[T: Specialized](x: T):
    def foo = x

def f(b: Foo[Int]) = 37 + b.foo
 
object Test:
    def main(args: Array[String]): Unit = {
        val x = new Foo[Int](42) {}
        val y = f(x)
        assert(y == 79)
    }
