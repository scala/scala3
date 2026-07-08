object i4370 {
  class Foo { type R = A }
  type A = List[Foo#R] // error: cyclic
}
