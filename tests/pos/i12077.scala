trait Wrapper[K]
trait Has0[T]

def test[R](v: Wrapper[Has0[String] with R]):R = ???

val zz:Wrapper[Has0[String] with Has0[Int]] = ???
val _ = test(zz)
