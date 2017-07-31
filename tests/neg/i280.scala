object infpaths3 {

  object a {
    trait T { t =>
      type M <: t.g.N // error
      type T <: a.T
      val f: t.T

      trait U { u =>
        type N <: t.f.M
        type U <: a.x.g.U // error
        val f: u.U
      }
      val g: t.U
    }
    val x: a.T = ???
  }

}
