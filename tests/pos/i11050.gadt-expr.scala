package pkg

case class Box(value: Int)

// Original: tests/run-custom-args/fatal-warnings/i11050.scala
// minimised to fix implementation of GadtExpr
class Test:
  def test: String = foo[Box]

  transparent inline def foo[T](using m: deriving.Mirror.Of[T]) =
    bar[m.MirroredElemLabels]

  transparent inline def bar[L] =
    inline new Tuple2(compiletime.erasedValue[L], 1) match
      case _: Tuple2[l *: _, _] =>
        compiletime.constValue[l].toString
