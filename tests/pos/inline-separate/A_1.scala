object A {
  inline def summon[T] = implicit match {
    case t: T => t
  }
}
