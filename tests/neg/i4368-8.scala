object Test8 {

  class A {
    type T = B#U  // error: cyclic
  }

  class B {
    type U = A#T
  }
}
