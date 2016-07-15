object i39neg {

  trait B {
    type D <: { type T }
    def d: D
  }

  val bc: B = new B {
    def d: D = ???
    private def pd: D = ???
  }

  val d: bc.D = bc.d
  val pd: bc.D = bc.pd             // error

  // infinite loop in Typer
  val asT: d.T = ???

}
