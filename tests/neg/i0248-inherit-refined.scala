object test {
  class A { type T }
  type X = A { type T = Int }
  class B extends X                                   // error
  type Y = A & B
  class C extends Y                                   // error
  type Z = A | B
  class D extends Z                                   // error
  abstract class E extends ({ val x: Int })           // error
}
