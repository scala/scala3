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
  def apply[T] with (fooT: Foo[T]) : Int = fooT.i

  given foo[T] with Low as Foo[T](0)
  given foobar[T] with Low as Foo[Bar[T]](1)
  given foobarbaz with Low as Foo[Bar[Baz]](2)
}
class Bar[T]
object Bar {
  given foobar[T] with Medium as Foo[Bar[T]](3)
  given foobarbaz with Medium as Foo[Bar[Baz]](4)
}
class Baz
object Baz {
  given baz with High as Foo[Bar[Baz]](5)
}

class Arg

given Arg

class Bam(val str: String)

given lo with Low : Bam("lo")

given hi with High with Arg : Bam("hi")

class Bam2(val str: String)

given lo2 with Low : Bam2("lo")

given mid2 with High with Arg : Bam2("mid")

given hi2 : Bam2("hi")

class Arg2
class Red(val str: String)

given normal with Arg2 : Red("normal")

given reduced with (ev: Arg2 | Low) : Red("reduced")

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