import lib.*

@main def Test: Unit =
  test(new Foo1(1))
  test(new Foo2(2, 3))
  test(new Foo3(4, 5))
  test(new Foo4(6, 7))

def test(any: Any): Unit =
  any match
    case Foo1(x) => println(s"Foo1($x)")
    case Foo2(x, y) => println(s"Foo2($x, $y)")
    case Foo3(x, y) => println(s"Foo3($x, $y)")
    case Foo4(x, y) => println(s"Foo4($x, $y)")
