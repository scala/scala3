object Utils {
  class Box[T]
  inline def foo[T](t: T) <: Any = inline t match {
    case _: Box[a] => scala.compiletime.constValue[a]
  }
}
