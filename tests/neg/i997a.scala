class C {

  class Super

  object O {

    private case class CC(x: Int)

    private implicit class D(x: Int) extends Super
  }

  import O.*

  println(O.CC(1)) // error: CC cannot be accessed

  val s: Super = 1 // error: type mismatch


}
