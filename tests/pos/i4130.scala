object Test {
  val f: (x: Int) => Int = x => x

  def id[A, B](x: (y: A) => B) = x

  id(f)
    // -- [E007] Type Mismatch Error: try/dep.scala:6:5 -------------------------------
    // 6 |  id(f)
    //   |     ^
    //   |     found:    (x: Int) => Int(Test.f)
    //   |     required: (y: Nothing) => Any
    //   |

  id[Int, Int](f) // OK

  trait A

  def foo[E](f: (a: A) => (a.type, E)): E = {
    val a = new A {}
    f(a)._2
  }

  foo { a => (a, ()) }
}