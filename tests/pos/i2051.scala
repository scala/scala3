class A[T](val x:T)
class B[T](override val x:T) extends A[T](x)
