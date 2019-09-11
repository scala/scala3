object A {
  inline def summon[T] = compiletime.summonFrom {
    case t: T => t
  }
}
