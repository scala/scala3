class Baz:
  transparent inline def foo: Int = bar(zero)
  transparent inline def bar(inline i: Int): Int = inline i match
    case 0 => 0
    case _ => scala.compiletime.error("XD")
  private transparent inline def zero = 0

@main
def Test =
  println(Baz().foo)