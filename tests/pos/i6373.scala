object Test {
  class Test  {
    class Context(val t: Boolean)

    type Contextual[T] = (given Context) => T

    inline def f(): Contextual[Boolean] = summon[Context].t

    given ctx : Context = new Context(true)

    f()
  }
}