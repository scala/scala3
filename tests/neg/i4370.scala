object App {
  class Foo { type R = A }
  type A = List[Foo#R] // error
}
