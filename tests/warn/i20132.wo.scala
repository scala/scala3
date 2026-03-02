sealed trait Foo[A]
case class Bar[C](x: Foo[C]) extends Foo[Int]
case class End[B]()          extends Foo[B]
class Test:
  def m1[M](foo: Foo[M]): Int = foo match
    case End()  => 0
    case Bar(_) => 1
  def t1 = m1[Int](Bar[Int](Bar[Int](End[Int]())))
