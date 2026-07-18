import language.experimental.modularity

trait A[T]:
  extension(t: T) def foo: T

trait B:
  type Self
  extension(t: Self) def foo: Self

given A[Int] = x => x // ok before

given Int is B = x => x // was: "cannot override an extension method"
