object App {
  class Foo { type R = A } // error
  type A = List[Foo#R]
}
