object templateParents {

  // traits do not call a constructor
  class C[+T](val x: T)
  trait D extends C[String]
  trait E extends C[Int]
  class F extends C[Boolean](true) {
    def foo = x
  }
  val cd = new C("abc") with D
  cd.x

}

object templateParents1 {
  // tests inference of synthesized class type
  class C[+T]
  trait D extends C[String]
  trait E extends C[Int]

  val x = new D with E

  val y: C[Int & String] = x
}

