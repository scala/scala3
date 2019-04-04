
// Test that flow inference behaves soundly within blocks.
// This means that flow facts are propagated to ValDefs, but not
// to DefDefs or lazy vals or implicit vals.
// The reason is that forward references are allowed for `defs`,
// so when we see
// ```
// f
// if (x == null) return "foo"
// def f = x.length
// ```
// we can't assume that f is executed _after_ the if statement
// is executed.
class Foo {

  def test1(): Unit = {
    val x: String|Null = ???
    if (x == null) return ()
    val y = x.length // ok: x: String inferred
    ()
  }

  def test2(): Unit = {
    val x: String|Null = ???
    if (x == null) return ()
    def y = x.length // error: x: String|Null inferred
    ()
  }

  def test3(): Unit = {
    val x: String|Null = ???
    if (x == null) return ()
    lazy val y = x.length // error: x: String|Null inferred
    ()
  }

  def test4(): Unit = {
    val x: String|Null = ???
    if (x == null) return ()
    implicit val y: Int = x.length // error: x: String|Null inferred
  }

  // This case is different from #3 because the type of y doesn't need
  // to be inferred, which triggers a different codepath within the completer.
  def test5(): Unit = {
    val x: String|Null = ???
    if (x == null) return ()
    lazy val y: Int = x.length // error: x: String|Null inferred
    ()
  }

  def test6(): Unit = {
    val x: String|Null = ???
    if (x == null) return ()
    def y: Int = x.length // error: x: String|Null inferred
    ()
  }

  // This test checks that flow facts are forgotten for defs, but only
  // the facts gathered within the current block are forgotten.
  // Other facts from outer blocks are remembered.
  def test7(): Unit = {
    val x: String|Null = ???
    if (x == null) {
    } else {
      def f = x.length       // ok
      def f2: Int = x.length // ok
    }
  }
}
