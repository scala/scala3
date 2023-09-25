class DependentPoly {

  sealed trait Col[V] {

    trait Wrapper
    val wrapper: Wrapper = ???
  }

  object Col1 extends Col[Int]

  object Col2 extends Col[Double]

  val polyFn: [C <: DependentPoly.this.Col[?]] => (x: C) => x.Wrapper =
    [C <: Col[?]] => (x: C) => (x.wrapper: x.Wrapper)
}

