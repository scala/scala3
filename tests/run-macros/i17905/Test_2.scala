@main def Test: Unit =
  println("case 1: " + testCtxParam { given String = "given"; def f(using t: String) = "placeholder"; f + " outside" })
  given String = "given"
  println("case 2: " + testCtxParam { def f(using t: String) = "placeholder"; f + " outside" })
  /* This is expected to match the first case. The current QuoteMatcher identifies a function with a contextual function. */
  println("case 3: " + testCtxParam { given i: String = "given"; def a(x: String) = "placeholder"; a(i) + " outside" } )
