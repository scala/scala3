// Exercise code paths for different types of cached term refs.
// Specifically, `NonNullTermRef`s are cached separately from regular `TermRefs`.
// If the two kinds of trefs weren't cached separately, then the code below would
// error out, because every time `x` is accessed the nullable or non-null denotation
// would replace the other one, causing errors during -Ychecks.

class Test {
  def foo(): Unit = {
    val x: String|Null = ??? // regular tref `x`
    if (x != null) {
      val y = x.length // non-null tref `x`
      x.length 	       // 2nd access to non-null tref `x`
      val z = x.length // 3rd access to non-null tref `x`
    } else {
      val y = x	       // regular tref `x`
    }
    val x2 = x	       // regular tref `x`
  }
}
