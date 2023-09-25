sealed trait A
case class Sub1() extends A
case object Sub2 extends A

def test(x: A | Null): Int =
  if x == null then return 0
  x match
    case Sub1() => 1
    case Sub2 => 2
