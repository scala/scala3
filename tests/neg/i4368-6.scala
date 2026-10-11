object Test6 {
  trait W { type T <: W; val t: T }
  trait X {
    type A = b.T
    val a : A = b.t
    type B <: W
    val b : B
  }
  trait Y {
    type A <: W
    val a : A
    type B = a.T
    val b = a.t
  }
  trait Z extends X with Y // error: cyclic
}
