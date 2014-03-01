object templateParentsNeg {

  class C(x: String)
  class C2
  trait D extends C("a") // error: traits may not call class constructors

  new C("b") with C2     // error: C2 is not a trait

}
