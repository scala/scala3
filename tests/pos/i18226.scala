trait F[-R]

trait Row[A]

def eliminateInt[R](f: F[R & Row[Int]]): F[R] = new F[R] {}

val x = new F[Row[Int] & Row[String]] {}

val _ = eliminateInt[Row[String]](x) // compiles OK when given explicit type
val y = eliminateInt(x) // was error
val _: F[Row[String]] = y