trait Bla[T]
object Bla {
  implicit def blaInt: Bla[Int] = new Bla[Int] {}
  implicit def blaString: Bla[String] = new Bla[String] {
    assert(false, "I should not be summoned!")
  }
}

trait ErasedFoo[FT]
object Test {
  type Foo[F[_], T] = ErasedFoo[F[T]]
  type Foo2[F[_], T] = Foo[F, T]

  def mkFoo[F[_], T](implicit gen: Bla[T]): Foo[F, T] = new Foo[F, T] {}
  def mkFoo2[F[_], T](implicit gen: Bla[T]): Foo2[F, T] = new Foo2[F, T] {}

  def main(args: Array[String]): Unit = {
    val a: Foo[[X] =>> (X, String), Int] = mkFoo
    val b: Foo2[[X] =>> (X, String), Int] = mkFoo
    val c: Foo[[X] =>> (X, String), Int] = mkFoo2
  }
}

