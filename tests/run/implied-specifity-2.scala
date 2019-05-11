class Low
object Low {
  implicit low for Low
}
class Medium extends Low
object Medium {
  implicit medium for Medium
}
class High extends Medium
object High {
  implicit high for High
}

class Foo[T](val i: Int)
object Foo {
  def apply[T] given (fooT: Foo[T]): Int = fooT.i

  implicit foo[T]    for Foo[T](0)        given Low
  implicit foobar[T] for Foo[Bar[T]](1)   given Low
  implicit foobarbaz for Foo[Bar[Baz]](2) given Low
}
class Bar[T]
object Bar {
  implicit foobar[T] for Foo[Bar[T]](3)   given Medium
  implicit foobarbaz for Foo[Bar[Baz]](4) given Medium
}
class Baz
object Baz {
  implicit baz for Foo[Bar[Baz]](5) given High
}

class Arg
implicit for Arg

class Bam(val str: String)
implicit lo for Bam("lo") given Low
implicit hi for Bam("hi") given High given Arg

class Bam2(val str: String)
implicit lo2 for Bam2("lo") given Low
implicit mid2 for Bam2("mid") given High given Arg
implicit hi2 for Bam2("hi")

class Arg2
class Red(val str: String)
implicit normal for Red("normal") given Arg2
implicit reduced for Red("reduced") given (ev: Arg2 | Low)

object Test extends App {
  assert(Foo[Int] == 0)
  assert(Foo[Bar[Int]] == 3)
  assert(Foo[Bar[Baz]] == 5)
  assert(the[Bam].str == "hi")
  assert(the[Bam2].str == "hi")
  assert(the[Red].str == "reduced")

  { implicit for Arg2
    assert(the[Red].str == "normal")
  }
}