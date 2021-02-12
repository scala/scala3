object A {
  transparent inline def summon[T] = compiletime.summonFrom {
    case t: T => t
  }
}
