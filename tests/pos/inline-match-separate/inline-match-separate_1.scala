object Utils {
  class Box[T]
  transparent inline def foo[T](t: T): Any = inline t match {
    case _: Box[a] => scala.compiletime.constValue[a]
  }
}
