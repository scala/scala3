object templateParents {

// traits do not call a constructor
  class C[+T](x: T)
  trait D extends C[String]
  trait E extends C[Int]
  new C("abc") with D

  //val x = new D with E

  //val y: C = x
}