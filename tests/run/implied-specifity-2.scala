class Low
object Low {
  implied low for Low
}
class Medium extends Low
object Medium {
  implied medium for Medium
}
class High extends Medium
object High {
  implied high for High
}

class Foo[T](val i: Int)
object Foo {
  def apply[T] given (fooT: Foo[T]): Int = fooT.i

  implied foo[T]    for Foo[T](0)        given Low
  implied foobar[T] for Foo[Bar[T]](1)   given Low
  implied foobarbaz for Foo[Bar[Baz]](2) given Low
}
class Bar[T]
object Bar {
  implied foobar[T] for Foo[Bar[T]](3)   given Medium
  implied foobarbaz for Foo[Bar[Baz]](4) given Medium
}
class Baz
object Baz {
  implied baz for Foo[Bar[Baz]](5) given High
}

class Arg
implied for Arg

class Bam(val str: String)
implied lo for Bam("lo") given Low
implied hi for Bam("hi") given High given Arg

class Bam2(val str: String)
implied lo2 for Bam2("lo") given Low
implied mid2 for Bam2("mid") given High given Arg
implied hi2 for Bam2("hi")

class Arg2
class Red(val str: String)
implied normal for Red("normal") given Arg2
implied reduced for Red("reduced") given (ev: Arg2 | Low)

object Test extends App {
  assert(Foo[Int] == 0)
  assert(Foo[Bar[Int]] == 3)
  assert(Foo[Bar[Baz]] == 5)
  assert(the[Bam].str == "hi")
  assert(the[Bam2].str == "hi")
  assert(the[Red].str == "reduced")

  { implied for Arg2
    assert(the[Red].str == "normal")
  }
}