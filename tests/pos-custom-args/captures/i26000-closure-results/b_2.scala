package i26000closureresults

import language.experimental.captureChecking

// Typer merges the declared parameter types of the two functions in the result types
// of the closures, so these parameter types are inferred
def use() =
  val f = (c: Boolean) => if c then Lib.m else Lib2.m2
  val h = (c: Boolean) => Box(if c then Lib.m else Lib2.m2)
  val k = (c: Boolean) => List(Lib.m, Lib2.m2)
