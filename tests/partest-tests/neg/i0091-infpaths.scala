object infpaths {

  object a {
    trait T { t =>
      type M <: t.b.M
      type T <: a.T
      val b: t.T
    }
    val x: a.T = ???
  }

  val m1: a.x.M = ???
  val m2: a.x.b.M = m1
  val m3: a.x.b.b.M = m2

}
