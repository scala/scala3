object test {
  class A { type T }
  type X = A { type T = Int }
  class B extends X
  type Y = A & B
  class C extends Y
  type Z = A | B
  class D extends Z
  abstract class A extends ({ val x: Int })
}
