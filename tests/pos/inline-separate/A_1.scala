object A {
  inline def summon[T] = implied match {
    case t: T => t
  }
}
