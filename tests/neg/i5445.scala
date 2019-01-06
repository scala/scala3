object Test {

  trait A { def polymorphic[x]: Int }
  val a = new A { val polymorphic = Unit } // error: object creation impossible

}