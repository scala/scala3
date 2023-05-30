class Foo[T <: Singleton](x: T)
class Outer
class Evil(val outer: Outer) extends Foo(outer) // error (because outer.type appears in the inferred type)
