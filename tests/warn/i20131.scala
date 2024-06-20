sealed trait Foo
case class Foo1() extends Foo
case class Foo2[A, B]() extends Foo

sealed trait Bar[A, B]
case class Bar1[A, C, D](a: Bar[C, D])         extends Bar[A, Bar[C, D]]
case class Bar2[   C, D](b: Bar[C, D], c: Foo) extends Bar[Bar1[Int, Byte, Int], Bar[C, D]]

class Test:
  def m1(bar: Bar[Bar1[Int, Byte, Int], Bar[Char, Char]]): Int = bar match
    case Bar1(_)         => 0
    case Bar2(_, Foo2()) => 1
  def t1 = m1(Bar2(null, Foo1()))
    // for Bar2[C, D] to match the scrutinee
    // C := Char and D := Char
    // which requires a Bar[Char, Char]
    // which isn't instantiable, outside of null
