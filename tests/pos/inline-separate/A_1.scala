object A {
  inline def summon[T] = delegate match {
    case t: T => t
  }
}
