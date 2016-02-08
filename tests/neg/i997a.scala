class C {

  class Super

  object O {

    private case class CC(x: Int)

    private implicit class D(x: Int) extends Super
  }

  import O._

  println(O.CC(1))

  val s: Super = 1


}
