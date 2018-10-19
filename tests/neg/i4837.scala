trait T(x: Int)

class C {
  type Foo[X] = T
  class D extends Foo[Unit] // error
}
