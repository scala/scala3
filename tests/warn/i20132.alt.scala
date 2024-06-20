sealed trait Foo[A]
case class Bar[C](x: Foo[C]) extends Foo[C]
case class End[B]()          extends Foo[B]
class Test:
  def m1[M](foo: Foo[M]): Int = foo match // warn: not exhaustive
    case End()      => 0
    case Bar(End()) => 1
  def t1 = m1[Int](Bar[Int](Bar[Int](End[Int]())))
