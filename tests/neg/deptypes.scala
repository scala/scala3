//> using options -language:experimental.dependent

type Vec[T] = (n: Int) =>> Array[T] // error: not yet implemented

type Matrix[T](m: Int, n: Int) = Vec[Vec[T](n)](m)  // error // error: not yet implemented

type Tensor2[T](m: Int)(n: Int) = Matrix[T](m, n)

val x: Vec[Int](10) = ???
val n = 10
type T = Vec[String](n)
