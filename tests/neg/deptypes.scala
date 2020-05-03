
type Vec[T] = (n: Int) =>> Array[T] // error: needs to be enabled
val x: Vec[Int](10) = ???  // error: needs to be enabled
val n = 10
type T = Vec[String](n)  // error: needs to be enabled