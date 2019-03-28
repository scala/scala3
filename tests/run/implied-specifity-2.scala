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

  implied foo[T]    given Low for Foo[T](0)
  implied foobar[T] given Low for Foo[Bar[T]](1)
  implied foobarbaz given Low for Foo[Bar[Baz]](2)
}
class Bar[T]
object Bar {
  implied foobar[T] given Medium for Foo[Bar[T]](3)
  implied foobarbaz given Medium for Foo[Bar[Baz]](4)
}
class Baz
object Baz {
  implied baz given High for Foo[Bar[Baz]](5)
}

class Arg
implied for Arg

class Bam(val str: String)
implied lo given Low for Bam("lo")
implied hi given High given Arg for Bam("hi")

class Bam2(val str: String)
implied lo2 given Low for Bam2("lo")
implied mid2 given High given Arg for Bam2("mid")
implied hi2 for Bam2("hi")

class Red(val str: String)
implied normal given Arg for Red("normal")
implied reduced given Arg given Low for Red("reduced")

object Test extends App {
  assert(Foo[Int] == 0)
  assert(Foo[Bar[Int]] == 3)
  assert(Foo[Bar[Baz]] == 5)
  assert(the[Bam].str == "hi")
  assert(the[Bam2].str == "hi")
  //assert(the[Red].str == "normal")
}