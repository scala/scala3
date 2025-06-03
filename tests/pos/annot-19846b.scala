class qualified[T](predicate: T => Boolean) extends annotation.StaticAnnotation

class EqualPair(val x: Int, val y: Int @qualified[Int](it => it == x))

@main def main =
  val p = EqualPair(42, 42)
  val y = p.y
  println(42)
