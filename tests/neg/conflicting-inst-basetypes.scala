
object Test:
  trait A[T]
  trait B1 extends A[Int]
  trait B2 extends A[String]
  class D extends B1, B2 // error: cannot be instantiated since it has conflicting base types Test.A[Int] and Test.A[String]
  // NOTE this is not accepted in Scala 2 either
