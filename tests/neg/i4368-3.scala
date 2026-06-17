object Test3 {
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

  object App {
    type Z = X with Y
    val z: Z = z
    val a: z.A = a // error: too deep
  }
}
