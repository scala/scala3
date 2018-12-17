trait M[-X] {
  def m(x: X): Boolean
}

class C
class A { class C }

object Test {
  def main(args: Array[String]): Unit = {
    val a = new A

    // class O extends M[a.C] { def m(x: a.C) = true }
    // (new O: M[Null]).m(null) // Okay

    ((a: A) => {
      class N extends M[a.C|Null] { def m(x: a.C|Null) = true }
      new N: M[Null]
    }).apply(a).m(null) // This used to be incorrectly marked as throwing an NPE.
  }
}
