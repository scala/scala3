// Test forward references handled with flow typing
// Currently, the flow typing will not be applied to definitions forwardly referred.

class Foo {

  def test0(): Unit = {
    def z: Int = y
    val x: String|Null = ???
    if (x == null) return ()
    def y: Int = x.length // error: x: String|Null inferred
  }


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
    def z: Int = y
    if (x == null) return ()
    def y: Int = x.length // error: x: String|Null is inferred
    ()
  }

  // Since y is referred before definition, flow typing is not used here.
  def test3(): Unit = {
    val x: String|Null = ???
    lazy val z = y
    if (x == null) return ()
    lazy val y = x.length // error: x: String|Null is inferred
    ()
  }

  // This case is invalid because z has an implicit forward reference to y,
  // but x, y and z aren't lazy (only forward references to lazy vals are allowed).
  // Since y is referred (by z) before definition, flow typing is not used here.
  // Only the typing error is shown because reference check is after typing.
  def test4(): Unit = {
    val z = implicitly[Int]
    val x: String|Null = ???
    if (x == null) return ()
    implicit val y: Int = x.length // error: x: String|Null inferred
  }

  // Since z is referred before definition, flow typing is not used here.
  def test5(): Unit = {
    val x: String|Null = ???
    if (x == null) return ()
    def y = z
    def z = x.length // error: x: String|Null inferred
  }
}