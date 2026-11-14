def foo(xs: Any*): Unit = ()

def test(p: Boolean, a: Array[Any], b: Array[Any]): Unit =
  foo(a*)
  // The spliced argument has a union of two array types
  foo((if p then a else b)*)
  // A local array, which is not read-only
  val c = Array[Any](1, 2)
  foo(c*)
  c(0) = 3
