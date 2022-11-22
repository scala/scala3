sealed trait TP:
  type C
  type P

final class Foo extends TP:
  class C
  enum P:
    case A, B

object Bar extends TP:
  class C
  enum P:
    case A, B, C

// Works
def test =
  summon[Foo#P <:< TP#P]
  val a: TP#P = Foo().P.A

  // These fail
  val b: TP#P = Bar.P.A: Bar.P
  summon[Bar.type#P <:< TP#P]
  summon[Bar.P <:< TP#P]
  val c: TP#C = ??? : Bar.C
