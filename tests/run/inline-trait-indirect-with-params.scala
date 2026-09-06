//> using options -language:experimental.inlineTraits
inline trait A[T](x: T):
    val y = x
    def foo() = x
inline trait B extends A[Int]
class C extends A[Int](10), B

object Test:
    def main(args: Array[String]): Unit = 
        val z: B = new C
        assert(z.foo() == 10)
        assert(z.y == 10)