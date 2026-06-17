object Test2 {
  trait W {
    type A
    type B
  }
  trait X { z: W =>
    type A = z.B
    type B
  }
  trait Y { z: W =>
    type A
    type B = z.A
  }
  trait Z extends X with Y // error: cyclic
}
