object Test extends App {
  trait A[+X]
  class B[+X](val x: X) extends A[X]
  class C[+X](x: Any) extends B[Any](x) with A[X] // error
}
