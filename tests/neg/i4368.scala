object Test1 {
  trait X {
    type A = B
    type B
  }
  trait Y {
    type A
    type B = A
  }
  trait Z extends X with Y // error: cyclic
}
