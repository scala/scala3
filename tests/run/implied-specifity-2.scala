class Low
object Low {
  given low: Low with {}
}
class Medium extends Low
object Medium {
  given medium: Medium with {}
}
class High extends Medium
object High {
  given high: High with {}
}

class Foo[T](val i: Int)
object Foo {
  def apply[T](using fooT: Foo[T]): Int = fooT.i

  given foo[T](using Low): Foo[T](0) with {}
  given foobar[T](using Low): Foo[Bar[T]](1) with {}
  given foobarbaz(using Low): Foo[Bar[Baz]](2) with {}
}
class Bar[T]
object Bar {
  given foobar[T](using Medium): Foo[Bar[T]](3) with {}
  given foobarbaz(using Medium): Foo[Bar[Baz]](4) with {}
}
class Baz
object Baz {
  given baz(using High): Foo[Bar[Baz]](5) with {}
}

class Arg

given Arg with {}

class Bam(val str: String)

given lo(using Low): Bam("lo") with {}

given hi(using High)(using Arg): Bam("hi") with {}

class Bam2(val str: String)

given lo2(using Low): Bam2("lo") with {}

given mid2(using High)(using Arg): Bam2("mid") with {}

given hi2: Bam2("hi") with {}

class Arg2
class Red(val str: String)

given normal(using Arg2): Red("normal") with {}

given reduced(using ev: Arg2 | Low): Red("reduced") with {}

object Test extends App {
  assert(Foo[Int] == 0)
  assert(Foo[Bar[Int]] == 3)
  assert(Foo[Bar[Baz]] == 5)
  assert(summon[Bam].str == "hi")
  assert(summon[Bam2].str == "hi")
  assert(summon[Red].str == "reduced")

  { given Arg2 with {}
    assert(summon[Red].str == "normal")
  }
}