object Test5 {
  trait X {
    type A = B
    type B
  }
  trait Y {
    type A
    type B = A
  }

  object App {
    type Z = X & Y
    val z: Z = z
    val a: z.A = a  // error: too deep
  }
}
