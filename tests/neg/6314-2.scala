object G {
  final class X
  final class Y

  opaque type Foo = Nothing // or X & Y
  object Foo {
    def apply[F[_]](fa: F[X & Foo]): F[Y & Foo] = fa
  }

  type Bar[A] = A match {
    case X => String
    case Y => Int
  }

  val a: Bar[X & Foo] = "hello"
  val b: Bar[Y & Foo] = 1 // error

  def main(args: Array[String]): Unit = {
    val a: Bar[X & Foo] = "hello"
    val i: Bar[Y & Foo] = Foo.apply[Bar](a)
    val b: Int = i // error
    println(b + 1)
  }
}
