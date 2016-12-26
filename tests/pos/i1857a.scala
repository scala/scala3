class Wrapper[Host]


class SubWrapper extends Wrapper[Foo]

class Foo
object Foo {
  implicit val fooWrapper: SubWrapper = new SubWrapper
}

class Inv[A, B]

object RunMe {
  def main(args: Array[String]): Unit = {
    def wrap1a[A, W <: Wrapper[Foo]](host: A)(implicit w: W): W = ???
    def wrap1b[A, W <: Wrapper[Foo]](host: A)(implicit w: W): A = ???
    def wrap1c[A, W <: Wrapper[Foo]](host: A)(implicit w: W): Inv[W, A] = ???
    def wrap1d[A, W <: Wrapper[Foo]](host: A)(implicit w: W): Nothing = ???

    def wrap2a[A, W <: Wrapper[A]](host: A)(implicit w: W): W = ???
    def wrap2b[A, W <: Wrapper[A]](host: A)(implicit w: W): A = ???
    def wrap2c[A, W <: Wrapper[A]](host: A)(implicit w: W): Inv[W, A] = ???
    def wrap2d[A, W <: Wrapper[A]](host: A)(implicit w: W): Nothing = ???

    val f: Foo = new Foo

    // work with master
    wrap1a(f)
    wrap1b(f)
    wrap1c(f)
    wrap1d(f)

    // do not work with master
    wrap2a(f)
    wrap2b(f)
    wrap2c(f)
    wrap2d(f)
  }
}
