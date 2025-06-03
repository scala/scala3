import language.experimental.namedTuples

class A:
  type T

class B extends A

val f: (x: A) => x.T = ???
val g: (x: B) => x.T = f // OK
val h: (x: A) => x.T = g // error
