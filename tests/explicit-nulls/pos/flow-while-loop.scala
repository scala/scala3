class C(val x: Int, val next: C|Null)

def f = {
  var xs: C|Null = C(1, C(2, null))
  while (xs != null) {
    val xsx: Int = xs.x
    // Currently, we can't track a path with a mutable variable prefix,
    // even though the variable is trackable, like (xs.next != null).
    val xscpy: C = xs
    if (xscpy.next != null) {
      val _: Int = xscpy.next.x
      if (xscpy.next.next != null) {
        val _: Int = xscpy.next.next.x
      }
    }
    xs = xs.next
  }
}
