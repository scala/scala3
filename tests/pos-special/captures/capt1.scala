class C
type Cap = C holds *

def f1(c: Cap): () => C holds c.type = () => c // ok

def foo() =
  val x: C holds * = ???
  val y: C holds x.type = x
  val x2: (() => C) holds x.type = ???
  val y2: () => C holds x.type = x2

  val z: () => Cap = f1(x)


