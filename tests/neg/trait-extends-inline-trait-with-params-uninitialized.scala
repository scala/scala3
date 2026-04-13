inline trait A[T](x: T):
    val y = x
trait B extends A[Int] // error: only parameterless inline traits may be extended by ordinary traits. 
class C extends B, A[Int](4)
