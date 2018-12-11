package example

class PrefC {
  type T
  def m1: T = ???

  object N {
    type U
  }
  def k1: N.U = ???
}

object PrefM {
  type T
  def n1: T = ???
}

object PrefO extends PrefC {
  def o1: T = ???
}

object PrefTest {
  val c: PrefC = ???
  def m2: c.T = ???
  def k2: c.N.U = ???
  import c.N._
  def k3: U = ???

  def n2: PrefM.T = ???

  import PrefM._
  def n3: T = ???
}
