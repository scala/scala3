package prefixes

class C {
  type T
  def m1: T = ???

  object N {
    type U
  }
  def k1: N.U = ???
}

object M {
  type T
  def n1: T = ???
}

object O extends C {
  def o1: T = ???
}

object Test {
  val c: C = ???
  def m2: c.T = ???
  def k2: c.N.U = ???
  import c.N.*
  def k3: U = ???

  def n2: M.T = ???

  import M.*
  def n3: T = ???
}
