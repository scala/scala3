package example

def greaterThan(x: Int, y: {z: Int with z > x}): Unit = ()

def qualifiedResult(x: Int, y: Int): {z: Int with z > x} = (x + y).runtimeChecked

def noQualifier(x: Int, y: Int): Unit = ()

def qualifiedSelf(x: {y: Int with y > 0}): Unit = ()

def impure(): Int = 42

def test() =
  // x is referred to from y's qualifier -> x should be lifted
  greaterThan(impure(), 3.runtimeChecked)
  // x is referred to from the result type qualifier -> x should be lifted
  qualifiedResult(impure(), 3)
  // no qualifiers at all -> nothing should be lifted
  noQualifier(impure(), 3)
  // x's qualifier refers to itself -> x should be lifted
  qualifiedSelf(impure().runtimeChecked)
