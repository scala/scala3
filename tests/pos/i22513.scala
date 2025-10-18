opaque type R[T] <: T = T

object Test {
  enum E:
    case A(a: Int)

  val v: R[E] = ???
  v match
    case E.A(_) =>
}

sealed trait Foo

case class FooA() extends Foo
case class FooB() extends Foo

object O {
  opaque type OpaqueFoo <: Foo = Foo
  def fooB(): OpaqueFoo = FooB()
}

@main def main =
  val p: O.OpaqueFoo = O.fooB()

  p match
    case _: FooA => println("fooA")
    case _: FooB => println("fooB")
