object A {
  inline def summon[T] = compiletime.summonInline[T]
}
