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
  def apply[T](using fooT: Foo[T]): Int = fooT.i

  given [T] => Low => Foo[T](0) as foo
  given [T] => Low => Foo[Bar[T]](1) as foobar
  given Low => Foo[Bar[Baz]](2) as foobarbaz
}
class Bar[T]
object Bar {
  given [T] => Medium => Foo[Bar[T]](3) as foobar
  given Medium => Foo[Bar[Baz]](4) as foobarbaz
}
class Baz
object Baz {
  given High => Foo[Bar[Baz]](5) as baz
}

class Arg

given Arg

class Bam(val str: String)

given Low => Bam("lo") as lo

given High => Arg => Bam("hi") as hi

class Bam2(val str: String)

given Low => Bam2("lo") as lo2

given High => (Arg) => Bam2("mid") as mid2

given hi2 as Bam2("hi")

class Arg2
class Red(val str: String)

given Arg2 => Red("normal") as normal

given (ev: Arg2 | Low) => Red("reduced") as reduced

object Test extends App {
  assert(Foo[Int] == 0)
  assert(Foo[Bar[Int]] == 3)
  assert(Foo[Bar[Baz]] == 5)
  assert(summon[Bam].str == "hi")
  assert(summon[Bam2].str == "hi")
  assert(summon[Red].str == "reduced")

  { given Arg2
    assert(summon[Red].str == "normal")
  }
}