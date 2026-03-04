object Test:
  trait A[T]
  trait B extends A[Int]
  trait C extends A[String]
  abstract class D extends B, C // error
  trait E extends B, C
