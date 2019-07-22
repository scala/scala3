class Low
object Low {
  given low as Low
}
class Medium extends Low
object Medium {
  given medium as Medium
}
class High extends Medium
object High {
  given high as High
}

class Foo[T](val i: Int)
object Foo {
  def apply[T] given (fooT: Foo[T]): Int = fooT.i

  given foo[T]    as Foo[T](0)        given Low
  given foobar[T] as Foo[Bar[T]](1)   given Low
  given foobarbaz as Foo[Bar[Baz]](2) given Low
}
class Bar[T]
object Bar {
  given foobar[T] as Foo[Bar[T]](3)   given Medium
  given foobarbaz as Foo[Bar[Baz]](4) given Medium
}
class Baz
object Baz {
  given baz as Foo[Bar[Baz]](5) given High
}

class Arg

given as Arg

class Bam(val str: String)
given lo as Bam("lo") given Low
given hi as Bam("hi") given High given Arg

class Bam2(val str: String)
given lo2 as Bam2("lo") given Low
given mid2 as Bam2("mid") given High given Arg
given hi2 as Bam2("hi")

class Arg2
class Red(val str: String)
given normal as Red("normal") given Arg2
given reduced as Red("reduced") given (ev: Arg2 | Low)

object Test extends App {
  assert(Foo[Int] == 0)
  assert(Foo[Bar[Int]] == 3)
  assert(Foo[Bar[Baz]] == 5)
  assert(the[Bam].str == "hi")
  assert(the[Bam2].str == "hi")
  assert(the[Red].str == "reduced")

  { given as Arg2
    assert(the[Red].str == "normal")
  }
}