// Test the `eq` and `ne` extension methods.
object Test {
  def main(args: Array[String]): Unit = {
    val x: String|Null = "hello"
    if (x ne null) {
      assert(x.length == 5)
    } else {
      assert(false)
    }

    val y: String|Null = null
    if (y eq null) {
      assert(true)
    } else {
      assert(y.length == 5)
    }

    class Foo {
      val sz: Int = 42
    }

    // Now test the `AnyRef` methods.
    val s: Foo = null.asInstanceOf[Foo]
    if (s eq null) {
      assert(true)
    } else {
      assert(false)
    }

    val s2 = new Foo
    if (s2 ne null) {
      assert(true)
    } else {
      assert(false)
    }

    // Now test the extension methods on null itself
    assert(null eq null)
    assert(!(null ne null))
    assert(null ne "hello")
    assert(null eq y)
    assert(null ne x)
  }
}
