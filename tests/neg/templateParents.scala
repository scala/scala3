object templateParentsNeg {

  class C(x: String)
  class C2
  trait D extends C("a") // error: traits may not call class constructors

  new C("b") with C2     // error: C2 is not a trait

}
object templateParentsNeg1 {
  class C[T]
  trait D extends C[String]
  trait E extends C[Int]

  val x = new D with E // error: conflicting type arguments inferred type
}
