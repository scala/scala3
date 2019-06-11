class Low
object Low {
  delegate low for Low
}
class Medium extends Low
object Medium {
  delegate medium for Medium
}
class High extends Medium
object High {
  delegate high for High
}

class Foo[T](val i: Int)
object Foo {
  def apply[T] given (fooT: Foo[T]): Int = fooT.i

  delegate foo[T]    for Foo[T](0)        given Low
  delegate foobar[T] for Foo[Bar[T]](1)   given Low
  delegate foobarbaz for Foo[Bar[Baz]](2) given Low
}
class Bar[T]
object Bar {
  delegate foobar[T] for Foo[Bar[T]](3)   given Medium
  delegate foobarbaz for Foo[Bar[Baz]](4) given Medium
}
class Baz
object Baz {
  delegate baz for Foo[Bar[Baz]](5) given High
}

class Arg
delegate for Arg

class Bam(val str: String)
delegate lo for Bam("lo") given Low
delegate hi for Bam("hi") given High given Arg

class Bam2(val str: String)
delegate lo2 for Bam2("lo") given Low
delegate mid2 for Bam2("mid") given High given Arg
delegate hi2 for Bam2("hi")

class Arg2
class Red(val str: String)
delegate normal for Red("normal") given Arg2
delegate reduced for Red("reduced") given (ev: Arg2 | Low)

object Test extends App {
  assert(Foo[Int] == 0)
  assert(Foo[Bar[Int]] == 3)
  assert(Foo[Bar[Baz]] == 5)
  assert(the[Bam].str == "hi")
  assert(the[Bam2].str == "hi")
  assert(the[Red].str == "reduced")

  { delegate for Arg2
    assert(the[Red].str == "normal")
  }
}