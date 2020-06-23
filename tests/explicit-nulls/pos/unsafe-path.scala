class S {
  class O {
    type I = Int
    val a: I = 1

    type S = String | Null
    val s: S = ""
  }

  locally {
    import scala.language.unsafeNulls

    val m: O | Null = new O
    val n: m.I = m.a

    val s1: m.S = m.s
    val s2: m.S | Null = m.s
    val ss1: String = s1
    val ss2: String = s2
  }
}