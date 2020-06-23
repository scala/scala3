class S {
  class O {
    type I = Int
    val a: I = 1

    type S = String | Null
    val s: S = ""
  }

  def f = {
    val o: O = new O
    val m: O | Null = o
    val n0: o.I = o.a
    val n1: m.I = 0 // error
    val n2: Int = m.a // error

    val s1: m.S = ??? // error
    val s2: m.S | Null = ??? // error
    val s3: String = m.s // error
    val ss: String = o.s // error
  }
}