class hierarOverload {
  trait AB {
    type TB
    protected trait A { val entities: List[TB] }
    protected trait B
  }
  object NAnB {
    type TB = nB
    type TA = nA
    class nA { List[nB]() }
    class nB {}
  }
  def foo = { val t = new NAnB.TB() }
}
class hierarOverload2 {
  object NAnB {
    type TB = nB
    class nB
  }
  def foo = { val t = new NAnB.TB() }
}
