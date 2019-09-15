class Low
object Low {
  given low : Low
}
class Medium extends Low
object Medium {
  given medium : Medium
}
class High extends Medium
object High {
  given high : High
}

class Foo[T](val i: Int)
object Foo {
  def apply[T] given (fooT: Foo[T]): Int = fooT.i

  given foo[T](given Low)    : Foo[T](0)
  given foobar[T](given Low) : Foo[Bar[T]](1)
  given foobarbaz(given Low) : Foo[Bar[Baz]](2)
}
class Bar[T]
object Bar {
  given foobar[T](given Medium): Foo[Bar[T]](3)
  given foobarbaz(given Medium): Foo[Bar[Baz]](4)
}
class Baz
object Baz {
  given baz(given High): Foo[Bar[Baz]](5)
}

class Arg

given Arg

class Bam(val str: String)

given lo(given Low): Bam("lo")

given hi(given High)(given Arg): Bam("hi")

class Bam2(val str: String)

given lo2(given Low) : Bam2("lo")

given mid2(given High)(given Arg) : Bam2("mid")

given hi2 : Bam2("hi")

class Arg2
class Red(val str: String)

given normal(given Arg2) : Red("normal")

given reduced(given ev: Arg2 | Low) : Red("reduced")

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