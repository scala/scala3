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

  given foo[T](using Low) as Foo[T](0)
  given foobar[T](using Low) as Foo[Bar[T]](1)
  given foobarbaz(using Low) as Foo[Bar[Baz]](2)
}
class Bar[T]
object Bar {
  given foobar[T](using Medium) as Foo[Bar[T]](3)
  given foobarbaz(using Medium) as Foo[Bar[Baz]](4)
}
class Baz
object Baz {
  given baz(using High) as Foo[Bar[Baz]](5)
}

class Arg

given Arg

class Bam(val str: String)

given lo(using Low) as Bam("lo")

given hi(using High)(using Arg) as Bam("hi")

class Bam2(val str: String)

given lo2(using Low) as Bam2("lo")

given mid2(using High)(using Arg) as Bam2("mid")

given hi2 as Bam2("hi")

class Arg2
class Red(val str: String)

given normal(using Arg2) as Red("normal")

given reduced(using ev: Arg2 | Low) as Red("reduced")

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