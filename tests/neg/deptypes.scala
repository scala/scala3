
type Vec[T] = (n: Int) =>> Array[T] // error: needs to be enabled

type Matrix[T](m: Int, n: Int) = Vec[Vec[T](n)](m)  // error: needs to be enabled

type Tensor2[T](m: Int)(n: Int) = Matrix[T](m, n)   // error: needs to be enabled

val x: Vec[Int](10) = ???  // error: needs to be enabled
val n = 10
type T = Vec[String](n)  // error: needs to be enabled