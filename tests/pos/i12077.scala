trait Wrapper[K]
trait Has0[T]

def test[R](v: Wrapper[Has0[String] & R]):R = ???

val zz:Wrapper[Has0[String] & Has0[Int]] = ???
val _ = test(zz)
