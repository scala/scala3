object A {
  inline def a[T](x: T): T = inline x match {
    case 1 => x
  }
}
