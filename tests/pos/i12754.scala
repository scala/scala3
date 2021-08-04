transparent inline def transform(inline a: Any): Any = inline a match {
  case x: Byte   => x
  case x: Short  => x
  case x: Int    => x
  case x: Long   => x
  case x: Float  => x
  case x: Double => x
  case _         => a
}

def test = {
  println(transform(0) < 5)
}
