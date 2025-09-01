infix enum Foo[A, B]:
  case C1 extends Foo[Int, Int]

val x: Int Foo Int = Foo.C1

