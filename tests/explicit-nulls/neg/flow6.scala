class Foo {

  def test1(): Unit = {
    val x: String|Null = ???
    if (x == null) return ()
    def y = x.length // ok: x: String inferred
    ()
  }

  // This test is similar to test1, but a forward DefDef referring
  // to y is added after x. y is completed before knowing the
  // fact "x != null", hence, the type x: String|Null is used.
  def test2(): Unit = {
    val x: String|Null = ???
    def z = y
    if (x == null) return ()
    def y = x.length // error: x: String|Null is inferred
    ()
  }

  def test3(): Unit = {
    val x: String|Null = ???
    lazy val z = y
    if (x == null) return ()
    lazy val y = x.length // error: x: String|Null is inferred
    ()
  }
}