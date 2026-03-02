// Regression test for https://github.com/mbovel/dotty/issues/9

def foo(x: Int, y: {v: Int with v > x}): Unit = ()

@main def TestInt: Unit =
  // ok
  foo(1, 2.runtimeChecked)

  // ok
  val p: Int = 1
  foo(p, 2.runtimeChecked)

  // was crash
  foo((1: Int), 2.runtimeChecked)
