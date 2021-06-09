transparent inline def transform(inline a: Any): Any = inline a match {
  case x: Byte   => x
  case x: Short  => x
  case x: Int    => x
  case x: Long   => x
  case x: Float  => x
  case x: Double => x
  case _         => a
}

inline def lt(inline a: Any, inline b: Double): Boolean = transform(a) < b // error

def test = {
  println(lt(0, 5))
}
