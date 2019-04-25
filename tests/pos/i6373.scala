object Test {
  class Test  {
    class Context(val t: Boolean)

    type Contextual[T] = given Context => T

    inline def f(): Contextual[Boolean] = the[Context].t

    implied ctx for Context = new Context(true)

    f()
  }
}