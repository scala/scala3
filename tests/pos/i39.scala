object i39 {

  trait B {
    type D <: { type T }
    def d: D
  }

  val bc: B = new B {
    def d: D = ???
  }

  val d: bc.D = bc.d

  // infinite loop in Typer
  val asT: d.T = ???

}
