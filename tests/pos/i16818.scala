class Foo
class U1 extends Foo
class U2 extends Foo
class U3 extends Foo
class U4 extends Foo
type U = U1 | U2
type U34 = U3 | U4
class Boxx[T](x: T*)
class MyPair[T](a: T, b: T)

@main def test =
  val u : U1 | U2 = ???
  val c: Boolean = ???
  val x = if (c) 0 else "0"
  val u34 : U3 | U4 = ???

  val b123 = Boxx(u, u34)
  val b123Test: Boxx[U | U34] = b123 //error

  val p = MyPair(u, u34)
  val p2: MyPair[U | U34] = p // error