//> using options -language:experimental.inlineTraits
inline trait A[T](x: T):
    def y = x

inline trait B extends A[Int]
inline trait D extends A[Int]

inline trait E extends B
inline trait F extends D

class C extends E, F, A[Int](100)

object Test:
    def main(args: Array[String]): Unit = 
        val z = new C
        assert(z.y == 100)
