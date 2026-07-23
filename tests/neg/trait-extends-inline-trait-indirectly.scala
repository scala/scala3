//> using options -language:experimental.inlineTraits
inline trait A[T](x: T):
    val y = x
inline trait B extends A[Int]
class C extends B // error: parameterized trait A is indirectly implemented, needs to be implemented directly so that arguments can be passed

object Test:
    def main(args: Array[String]): Unit = 
        val z = new C
