package internal:
  object Foo:
    inline def foo: Unit = P.s

    private object P { def s = "b" }
end internal

@main def Test = internal.Foo.foo