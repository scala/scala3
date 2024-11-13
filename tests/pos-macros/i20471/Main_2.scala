import scala.annotation.experimental

@experimental
object autoFlatMapTests:
  trait TestAlgebra[T] derives FlatMap:
    def abstractEffect(a: String): T
    def concreteEffect(a: String): T = abstractEffect(a + " concreteEffect")
