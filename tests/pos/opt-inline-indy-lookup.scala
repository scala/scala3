//> using options -opt -opt-inline:**

// Inlining library code brings in an invokedynamic whose bootstrap handle refers to
// MethodHandles$Lookup, a nested class codegen had no ClassBType for (None.get in getClassIfNested).
class F extends (Int => Int):
  def apply(x: Int) = x
def t = (new F)(1)
